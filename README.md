# Camera Control
This is a simple application to control a camera adhering to the Sony VISCA standard.<br>
Note the RS232 pinout in the Tandberg, and later Cisco, manuals is wrong. Instead of pin 3 from the RJ45 going to pin 2 on the D-SUB, and pin 6 on the RJ45 going to pin 3 on the D-SUB, it should be the other way round:<br>
RJ45 pin 3 (RX) -> D-SUB pin 3 (TX)<br>
RJ45 pin 6 (TX) -> D-SUB pin 2 (RX)<br>
<br>
Project was written in Lazarus (https://www.lazarus-ide.org). I have supplied binaries for Windows, macOS, Linux and RaspbianOS but I have only tested the actual hardware on my Windows laptop connected to a Tandberg PrecisionHD VC camera. Full source is available if you wish to compile for other systems.<br>
<br>
If you want, you can support this project, and others, by buying me a coffee (or a tea/beer/rum/etc.): https://ko-fi.com/geraldholdsworth<br>
