function highlight(line_numbers)
  local highlighter = {
    Div = function(div)
      if div.classes:includes('output') then
        local output_lines = {}
        for line in string.gmatch(div.text, "[^\r\n]+") do
          table.insert(output_lines, line)
        end
        local highlighted_lines = {}
        for i, line in ipairs(output_lines) do
          if line_numbers:find(tostring(i), 1, true) then
            table.insert(highlighted_lines, '<span class="highlight">' .. line .. '</span>')
          else
            table.insert(highlighted_lines, line)
          end
        end
        div.text = table.concat(highlighted_lines, '\n')
        return div
      end
    end
  }
  return highlighter
end

function Pandoc(doc)
  if FORMAT == 'revealjs' then
    local doc_text = pandoc.utils.stringify(doc)
    local line_numbers = doc.meta["output-line-numbers"]
    return pandoc.walk_block(pandoc.Div(pandoc.RawBlock('html', doc_text)), highlight(line_numbers))
  end
end

