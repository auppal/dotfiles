--
-- Look_cleanviolet for Notion's default drawing engine. 
-- Based on look-clean and look-violetgrey.
-- 

if not gr.select_engine("de") then
    return
end

-- Clear existing styles from memory.
de.reset()

-- Base style
de.defstyle("*", {
    -- Gray background
    highlight_colour = "#eeeeee",
    shadow_colour = "#eeeeee",
    background_colour = "#aaaaaa",
    foreground_colour = "#000000",
    
    shadow_pixels = 1,
    highlight_pixels = 1,
    padding_pixels = 0,
    spacing = 0,
    border_style = "elevated",
    
    font = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*",
    text_align = "center",
    transparent_background = 1,
})


de.defstyle("tab", {
    --font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*",
    font = "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-*-*",
    --font = "6x10",
    
    de.substyle("active-selected", {
        -- Violet tab
        -- highlight_colour = "#aaaacc",
        -- shadow_colour = "#aaaacc",
        -- background_colour = "#666699",
        -- foreground_colour = "#eeeeee",
        -- Orange tab            
        -- shadow_colour = "#f0f066",
        -- highlight_colour = "#f0f066",
        -- background_colour = "#f0c000",
        shadow_colour = "#ff6600",
        background_colour = "#ff6600",
        highlight_colour = "#ff6600",
        foreground_colour = "#000000",        
    }),

    de.substyle("inactive-selected", {
        -- Greyish violet tab
        highlight_colour = "#eeeeff",
        shadow_colour = "#eeeeff",
        background_colour = "#9999aa",
        foreground_colour = "#000000",
    }),
})


de.defstyle("input", {
    text_align = "left",
    spacing = 1,
    -- font = "9x15",
    -- font = "LiberationMono",
    font = "10x20",

    -- Greyish violet background
    -- highlight_colour = "#eeeeff",
    -- shadow_colour = "#eeeeff",
    -- background_colour = "#9999aa",
    -- foreground_colour = "#000000",
    
    -- de.substyle("*-selection", {
    --     background_colour = "#777799",
    --     foreground_colour = "#000000",
    -- }),

    -- de.substyle("*-cursor", {
    --     background_colour = "#000000",
    --     foreground_colour = "#9999aa",
    -- }),

    highlight_colour = "#ff6600",
    shadow_colour = "#ff6600",
    background_colour = "#3f3f3f",
    foreground_colour = "#dcdccd",
    
    de.substyle("*-selection", {
        background_colour = "#777799",
        foreground_colour = "#dcdccd",
    }),

    de.substyle("*-cursor", {
        background_colour = "#dcdccd",
        foreground_colour = "#000000",
    }),
})


dopath("lookcommon_clean")


-- Refresh objects' brushes.
gr.refresh()
