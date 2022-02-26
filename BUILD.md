# Interactive

rlwrap sml -m sources.cm

rlwrap sml
CM.make "sources.cml";

# Executables

## SML/NJ Images

have main function call OS.Process.exit O
ml-build <cm-file> <main function> <output image>
sml @SMLload=imagefile arg1 arg2 arg3...

## MLton executable

mlton -default-ann 'allowExtendedTextConsts true' sources.mlb

