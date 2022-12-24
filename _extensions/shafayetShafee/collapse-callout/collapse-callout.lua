-- defining all possible callout types
local callouts_all = {
        caution = 'callout-caution', 
  			important = 'callout-important',
  			tip = 'callout-tip', 
  			note = 'callout-note', 
  			warning = 'callout-warning'
    }

-- function for adding collapse attributes to callout divs
function collapse_callout(callouts, bool)
  local callout_filter = {
    Div = function(el)
        for key, val in pairs(callouts) do
          if el.classes:includes(val) then
            if el.attributes["collapse"] == nil then
              el.attributes["collapse"] = bool
              return el
            end
          end
        end
    end
  }
  return callout_filter
end

-- make changes to input file if the format is html
if quarto.doc.isFormat("html:js") then
  function Pandoc (doc)
    local collapse = doc.meta['collapse-callout']
    if not collapse then
     return nil
    end
    
    if collapse.all == false then
      return doc:walk(collapse_callout(callouts_all,'false'))
    elseif collapse.all == true then
      return doc:walk(collapse_callout(callouts_all, 'true'))
    else
      filtered_doc = doc
      for k, v in pairs{"caution", "important", "tip", "note", "warning"} do
        if collapse[v] == true then
          filtered_doc = filtered_doc:walk(
            collapse_callout({callouts_all[v]}, 'true')
          )
        elseif collapse[v] == false then
          filtered_doc = filtered_doc:walk(
            collapse_callout({callouts_all[v]}, 'false')
          )
        end
      end
      return filtered_doc
    end
    return nil
  end
end