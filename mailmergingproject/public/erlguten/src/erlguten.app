%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, erlguten,
 [{description, "System for high-quality typesetting"},
  {vsn, "2.5.1"},
  {modules, [eg_afm, eg_convert, eg_embed, eg_font_server, eg_hyphenate, eg_lib, eg_line_break, eg_mk_hyphen, eg_pdf_analyse, eg_pdf_annot, eg_pdf_assemble, eg_pdf, eg_pdf_image, eg_pdf_lib, eg_pdf_obj, eg_pdf_op, eg_pdf_page, eg_richText2pdf, eg_richText, eg_xml2richText, eg_xml_lite, eg_xml_tokenise, erlguten]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
