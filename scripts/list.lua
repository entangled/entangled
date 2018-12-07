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

-- filename: list.lua
-- description: Pandoc filter that lists all "file" attributes given
--   with code block entries.

local files = {}

function CodeBlock (elem)
    for k, v in pairs(elem.attr[3]) do
        if k == "file" then
            files[v] = elem.text
        end
    end
    return nil
end

function Pandoc (elem)
    local content = {}
    for filename, code in pairs(files) do
        table.insert(content, pandoc.Str(filename .. "\n"))
    end
    return pandoc.Pandoc(pandoc.Plain(content))
end