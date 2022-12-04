@echo off

rem compress kosinski files
echo Compressing Kosinski files
for %%f in ("256x256 Mappings\*.unc") do "bin\koscmp.exe" "%%f" "256x256 Mappings\%%~nf.kos"
for %%f in ("Graphics Kosinski\*.bin") do "bin\koscmp.exe" "%%f" "Graphics Kosinski\%%~nf.kos"

rem compress nemesis files
echo Compressing Nemesis files
for %%f in ("Graphics Nemesis\*.bin") do "bin\nemcmp.exe" "%%f" "Graphics Nemesis\%%~nf.nem"

rem assemble final rom and generate symbol file
IF EXIST s1built.bin move /Y s1built.bin s1built.prev.bin >NUL

IF EXIST DAC Driver.unc move /Y DAC Driver.unc >NUL
"bin\axm68k.exe" /m /k /p _Main.asm, s1built.bin >errors.txt, sonic.sym , _Main.lst
type errors.txt
if not exist s1built.bin pause & exit

rem compress and insert DAC driver
"bin\DualPCM_Compress.exe" "sound\DAC Driver.unc" "sound\DAC Driver Offset & Size.dat" s1built.bin "bin\koscmp.exe"

rem encode symbols and append to end of assembled rom
IF EXIST s1built.bin convsym sonic.lst s1built.bin -input asm68k_lst -inopt "/localSign=@ /localJoin=. /ignoreMacroDefs+ /ignoreMacroExp- /addMacrosAsOpcodes+" -a

rem check for success and fix header
IF NOT EXIST s1built.bin PAUSE & EXIT 2

"bin\fixheadr.exe" s1built.bin s1built.bin
pause

