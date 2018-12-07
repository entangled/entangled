local vars = {}

function CodeBlock (elem)
    title = nil
    do_annotate = false

    if elem.identifier and elem.identifier ~= "" then
        if vars[elem.identifier] then
            title = pandoc.Str ("«" .. elem.identifier .. "»=+")
        else
            vars[elem.identifier] = true
            title = pandoc.Str ("«" .. elem.identifier .. "»=")
        end
        do_annotate = true
    end

    for k, v in pairs(elem.attr[3]) do
        if k == "file" then
            title = pandoc.Str ("file: «" .. v .. "»=")
            do_annotate = true
        end
    end
    elem.attr[3] = {}

    if do_annotate then
        return { pandoc.Para {pandoc.Emph (title)},  elem }
    else
        return elem
    end
end