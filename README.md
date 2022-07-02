# 8008-SBC
Home-brew Intel 8008 single board computer with SCELBAL BASIC interpreter in EPROM. The hardware design borrows heavily from Len Bayles's 
[Intel 8008 Computer Clock](http://www.8008chron.com/) and Jim Kearney's [Tiny 8](https://www.jkearney.com/Tiny8demo/). The SCELBAL BASIC Interpreter can be found at Mike Willegal's [SCELBAL BASIC](http://www.willegal.net/scelbi/scelbal.html) page. The firmware was assembled using the [Macro Assembler AS](http://john.ccac.rwth-aachen.de:8000/as/). One half of a 27256 32K EPROM is used to store the firmware. The SCELBAL BASIC object code is loaded into the EPROM from 0000H to 1FFFH. The monitor object code is loaded into the EPROM from 2000H to 3FFFH. A jumper connected to the A13 input of the EPROM is used to select either the SCELBAL BASIC or the monitor for execution by the 8008.
<p align="center"><img src="/images/8008 SBC.JPEG"/>
<p align="center">Home-brew Intel 8008 SBC</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-1.png"/>
<p align="center">8008 SBC Clock and Reset Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-2.png"/>
<p align="center">8008 SBC Address and Data Bus Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-3.png"/>
<p align="center">8008 SBC Memory Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-4.png"/>
<p align="center">8008 SBC G22V10 #1 Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-5.png"/>
<p align="center">8008 SBC G22V10 #2 Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-6.png"/>
<p align="center">8008 SBC Serial I/O Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-7.png"/>
<p align="center">8008 Parallel Output Schematic</p><br>
<p align="center"><img src="/images/8008 SBC Schematic-8.png"/>
<p align="center">8008 SBC Vcc and Gnd Connections</p><br>
