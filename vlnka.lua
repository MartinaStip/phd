function Inlines(inlines)
  local result = {}
  local i = 1
  
  while i <= #inlines do
    local el = inlines[i]
    
    -- Kontrola, jestli je to jednopísmenné slovo (předložka/spojka)
    if el.t == "Str" and el.text:match("^[aikosuvzAIKOSUVZ]$") then
      -- Podívej se na následující element
      if i + 1 <= #inlines and inlines[i + 1].t == "Space" then
        -- Nahraď mezeru nezlomitelnou mezerou
        table.insert(result, el)
        table.insert(result, pandoc.Str("\u{00A0}"))
        i = i + 2  -- Přeskoč mezeru
      else
        table.insert(result, el)
        i = i + 1
      end
    else
      table.insert(result, el)
      i = i + 1
    end
  end
  
  return pandoc.Inlines(result)
end
