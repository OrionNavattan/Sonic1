@echo off

rem assemble Z80 sound driver
axm68k /m /k /p "sound\DAC Driver.asm", "sound\DAC Driver.unc" >"sound\errors.txt", , "sound\DAC Driver.lst"
type "sound\errors.txt"
IF NOT EXIST "sound\DAC Driver.unc" PAUSE & EXIT 2

rem compress kosinski files
for %%f in ("256x256 Mappings\*.unc") do kosinski_compress "%%f" "256x256 Mappings\%%~nf.kos"
kosinski_compress "Graphics - Compressed\Ending Flowers.unc" "Graphics - Compressed\Ending Flowers.kos"
kosinski_compress "sound\DAC Driver.unc" "sound\DAC Driver.kos"

rem assemble final rom and generate symbol file
IF EXIST s1built.bin move /Y s1built.bin s1built.prev.bin >NUL
axm68k /m /k /p sonic.asm, s1built.bin >errors.txt, sonic.sym, sonic.lst
type errors.txt

rem encode symbols and append to end of assembled rom
IF EXIST s1built.bin convsym sonic.lst s1built.bin -input asm68k_lst -inopt "/localSign=@ /localJoin=. /ignoreMacroDefs+ /ignoreMacroExp- /addMacrosAsOpcodes+" -a

rem check for success and fix header
IF NOT EXIST s1built.bin PAUSE & EXIT 2
fixheadr.exe s1built.bin
pause