-- Copyright 2018 Johan Hidding

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- filename: tangle.lua
-- description: Pandoc filter that generates a Bash script that generates
--   files defined in the code blocks, thus *tangling* the contents.

local vars = {}
local files = {}
local preamble = [[
#!/bin/bash

prepare() {
    echo "$1"
    mkdir -p $(dirname $1)
}

echo "Tangling ... "

tangle_dir=$(mktemp -d /tmp/tangle.XXXXXXXXXX)
target_dir=$(pwd)

cd "${tangle_dir}"
]]

local postamble = [[
cd "${target_dir}"

echo -e "\nSyncronising source files ..."
rsync -vrcup ${tangle_dir}/* .
sync
rm -rf ${tangle_dir}
]]

function CodeBlock (elem)
    if elem.identifier then
        t = vars[elem.identifier] or ""
        vars[elem.identifier] = t .. "\n" .. elem.text
    end

    for k, v in pairs(elem.attr[3]) do
        if k == "file" then
            files[v] = elem.text
        end
    end
    return nil
end

function string:split(delimiter)
    local result = { }
    local from  = 1
    local delim_from, delim_to = string.find( self, delimiter, from  )
    while delim_from do
      table.insert( result, string.sub( self, from , delim_from-1 ) )
      from  = delim_to + 1
      delim_from, delim_to = string.find( self, delimiter, from  )
    end
    table.insert( result, string.sub( self, from  ) )
    return result
  end

function expandCode (pre, key)
    local x = ""
    for i, line in ipairs(vars[key]:split("\n")) do
        x = x .. pre .. line:gsub("(%s*)<<(%g+)>>", expandCode) .. "\n"
    end
    return x
end

function expandFile (key)
    local x = ""
    for i, line in ipairs(files[key]:split("\n")) do
        x = x .. line:gsub("(%s*)<<(%g+)>>", expandCode) .. "\n"
    end
    return x
end

function Pandoc (elem)
    local content = { pandoc.Str(preamble) }
    for filename, code in pairs(files) do
        code = "prepare " .. filename .. "\n" ..
               "cat > " .. filename .. " << EOF\n" ..
                expandFile(filename):gsub("\\", "\\\\"):gsub("%$", "\\$"):gsub("`", "\\`") ..
                "EOF\n\n"
        table.insert(content, pandoc.Str(code))
    end
    table.insert(content, pandoc.Str(postamble))
    return pandoc.Pandoc(pandoc.Plain(content))
end
