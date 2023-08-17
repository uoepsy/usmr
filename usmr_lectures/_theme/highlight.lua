-- Lua filter for Quarto/Reveal.js to highlight specified line numbers in code chunk output

-- Function to parse the "output-line-numbers" argument
local function parseLineNumbers(arg)
  local lineNumbers = {}
  for num in string.gmatch(arg, "%d+") do
    table.insert(lineNumbers, tonumber(num))
  end
  return lineNumbers
end

-- Function to generate CSS styles for highlighted line numbers
local function generateCSS(lineNumbers)
  local css = ""
  for _, num in ipairs(lineNumbers) do
    css = css .. ".highlight-line-" .. num .. "{background-color: yellow;}\n"
  end
  return css
end

-- Function to modify code block output
local function modifyCodeBlock(block)
  -- Check if "output-line-numbers" is specified in the code chunk's attributes
  local attr = block.attributes
  if attr and attr["output-line-numbers"] then
    local lineNumbers = parseLineNumbers(attr["output-line-numbers"])
    local css = generateCSS(lineNumbers)
    -- Add the generated CSS to the document's header
    pandoc.utils.stringify(pandoc.Meta{pandoc.MetaInlines{pandoc.RawInline("html", "<style>" .. css .. "</style>")}}, {wrap = false})
    -- Wrap each output line with a span to apply the highlight
    for i in ipairs(block.content) do
      block.content[i] = pandoc.walk_block(block.content[i], {
        Str = function(el)
          return pandoc.Span({ pandoc.Str(el.text) }, { class = "highlight-line-" .. i })
        end
      })
    end
  end
  return block
end

return {
  {
    CodeBlock = modifyCodeBlock,
  }
}
