# 8008-SBC
Home-brew Intel 8008 single board computer with SCELBAL BASIC interpreter in EPROM. The hardware design borrows heavily from Len Bayles's 
[Intel 8008 Computer Clock](http://www.8008chron.com/) and Jim Kearney's [Tiny 8](https://www.jkearney.com/Tiny8demo/). The SCELBAL BASIC Interpreter can be found at Mike Willegal's [SCELBAL BASIC](http://www.willegal.net/scelbi/scelbal.html) page. The firmware was assembled using the [Macro Assembler AS](http://john.ccac.rwth-aachen.de:8000/as/).

Version 2 of the SBC uses 16K of a 32K 27256 EPROM. the Serial Monitor occupies the first 8K of the EPROM. The Scelbi BASIC Interpreter (SCELBAL) occupies the second 8K of the EPROM. When reset, the SBC starts executing code from the first 8K of EPROM (Monitor). When 'S' is entered at the Monitor's prompt, the SBC 'bank switches' to select the second 8K of the EPROM (SCELBAL).

Assemble both 'monitor.asm' and 'scelbal-in-eprom.asm' with the AS Macro Assembler. Use the assembler's P2BIN utility to generate two binary files. Concatenate the resulting two binary files 
into one file for your EPROM programmer. One way is to use the DOS 'COPY' command: 'copy /b monitor.bin + scelbal-in-eprom.bin eprom.bin'. I prefer to use the concatenate function in the [Freeware Hex Editor HxD](https://mh-nexus.de/en/hxd/) by Maël Hörz.

<p align="center"><img src="/images/8008 SBC.JPEG"/>
<p align="center">Home-brew Intel 8008 SBC</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-1.jpg"/>
<p align="center">8008 SBC Clock and Reset Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-2.jpg"/>
<p align="center">8008 SBC Address and Data Bus Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-3.jpg"/>
<p align="center">8008 SBC Memory Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-4.jpg"/>
<p align="center">8008 SBC G22V10 #1 Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-5.jpg"/>
<p align="center">8008 SBC G22V10 #2 Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-6.jpg"/>
<p align="center">8008 SBC Serial I/O Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-7.jpg"/>
<p align="center">8008 Parallel Output Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-8.jpg"/>
<p align="center">8008 SBC Vcc and Gnd Connections</p><br>
