rlwrap sml -m sources.cm

rlwrap sml
CM.make "sources.cml";

mlton -default-ann 'allowExtendedTextConsts true' sources.mlb