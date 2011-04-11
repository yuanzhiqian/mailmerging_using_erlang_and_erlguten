-record(frame_info, {name, 
                     class,
                     x,
                     y,
                     width,
                     height,
                     grid,
                     bg,
                     font,
                     fontsize,
                     paraIndent,
                     maxlines,
                     continue,
                     break}).

-record(template_info,{counts = 0,                             %%    1,2,3
                       page_amount_needed = 0,                        %%    counts must be 3 if page_needed is not less than 3
                       front_paper = undefined,                %%    {paper, _, [{frame_info, Content}, {frame_info, Content} ....]
                       middle_paper = undefined,               %%    {paper, _, [{frame_info, Content}, {frame_info, Content} ....]
                       end_paper = undefined}).                %%    {paper, _, [{frame_info, Content}, {frame_info, Content} ....]
