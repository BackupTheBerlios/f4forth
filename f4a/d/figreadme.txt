ARCHIVE : figdoc.zip
  This documentation is dated 1979:may. 
  OCR-ed by A.van der Horst, DFW, The Netherlands (2000:mar)
  Correction of typo's (some original) and slight changes to the layout are
  the only changes. 

Description
The general documentation for figForth supplements several
Forths published by the Forth Interest Group , in particular
fig86.zip that is available i.a. from Simtel. 

Content of files
figdoc.txt      Installation Manual
frontpage.tif   Front Page of the Installation Manual
		bloedsinn => .tif vermurkst, ..ps-umwandlung 2MB fuer nix -> raus
memmap.tif      Memory Lay out, fig. in Manual
		dto, idiotisches format, das .tif, => jpg => text s.u. (below)
glossary.txt    Reference manual for the Forth commands

Contact
Comments and improvements are welcome at the address below.
The web site contains figdoc.zip and modern implementations
of the fig Model, such as for linux and 32 bit MSDOS.

Albert van der Horst,Oranjestr 8,3511 RA UTRECHT,THE NETHERLANDS
albert@spenarnc.xs4all.nl     http://home.hccnet.nl/a.w.m.van.der.horst

========================================================================================
memmap:

standard fig-forth memory map			6502 memory map

limit -> --------------- <- use			limit -> --------------- <- use	
	  disc buffers					  disc buffers		
first -> --------------- <- prev		first -> --------------- <- prev
	 ---------------				 ---------------	
	 user area					 user area		
up ->	 ---------------			up ->	 ---------------	

r0 ->	 --------------- 				 ---------------		
	 return        / \				 text buffer			
	 stack      /     \				 --------------- <- pad		
                 /         ) in				 "word" buffer			
	      / terminal  /			dp ->	 ---------------		
	   /    buffer   /				 dictionary			
rp ->	 --------------- <- tib				 ---------------		
							 ---------------		
s0 ->	 ---------------				 boot-up literals		
	 stack						 --------------- <- 0 +origin	
sp ->	 ---------------			
						
						
	 ---------------			r0 ->	 --------------- 		
	 text buffer					 return        / \		
	 --------------- <- pad				 stack      /     \		
	 "word" buffer				                 /         ) in		
dp ->	 ---------------				      / terminal  /		
	 dictionary					   /    buffer   /		
	 ---------------			rp ->	 --------------- <- tib		
	 ---------------			
	 boot-up literals			
	 --------------- <- 0 +origin		Z-page	 ---------------
	                                                  UP N IP W
							 ---------------
	
	                                        s0 ->	 ---------------			
	                                         	 stack					
	                                        sp ->	 ---------------			

========================================================================================
