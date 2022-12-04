Sonic the Hedgehog (Mega Drive) Disassembly
===========================================

Differences with the disassembly on the [Sonic Retro Github page](https://github.com/sonicretro/s1disasm):

* __dma__ macro - Replaces writeVRAM and writeCRAM macros.
* __gmptr__ macro - Generates ids for game modes.
* __index__ & __ptr__ macros - Creates relative and absolute pointer lists; automatically generates id numbers.
* __lsline__ macro - Allows level select menu strings to be stored as plain ascii.
* __sonic_sprites__ macro - The index for Sonic's mappings is reused for DPLCs, keeping them both congruous.
* __filedef__ & __incfile__ macros - Records the file name and decompressed size of graphics for VRAM management.
* __objpos__ macro - Object placement in levels, also uses object ids instead of fixed numbers.
* __plcm__ macro - Generates tile ids for VRAM addresses (e.g. tile_Nem_Ring). Automatically places graphics after previous graphics, if no VRAM address is specified.
* __spritemap__ & __piece__ macros - Creates sprite mappings.
* Z80 macros - See [axm68k](https://github.com/cvghivebrain/axm68k).
* Different labels used for object status table.
  * Constants used for render and status flags.
* Different labels used for some RAM addresses and routines.
  * Deprecated labels, as well as those from other games, are stored in [a compatibility file](Includes/Compatibility.asm).
* Automatic recompression of Kosinski-compressed data. Thanks to [Clownacy](https://github.com/Clownacy) for [the compressor](https://github.com/Clownacy/accurate-kosinski).
* Automatic RAM management.
  * Prevents word/longword variables starting on an odd address.
  * Prevents accidental clearing of bytes after the end of a cleared section (which are always in multiples of 4 bytes).
  * Indicates how much RAM is used (and unused) in errors.txt.
* DAC driver is compiled, compressed and inserted seamlessly. Thanks to [AuroraFields](https://github.com/NatsumiFox) for [the Dual PCM tool](https://github.com/NatsumiFox/Dual-PCM-Compress).