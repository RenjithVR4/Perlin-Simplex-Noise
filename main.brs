'an attempt to make use of the Perlin / Simplex noise functions that I've ported. 
'buggy but I don't really know what i'm doing with Perlin noise
'this project started in 2011 and every so often I attempt to make some headway with the conversion

sub main()

init=simplexnoise()
screen=createobject("roscreen")


for y=0 to 300  
	for x=0 to 300 
		'wanted to do 2D perlin noise but the original function was broken so I'm stuck with 3D noise
		'what the heck do I do with the Z variable that I don't really want to use?
    'lets try this for fun...
		z=x+y 
		
		
		color=init.nze(x,y,z)
		'ff is an attempt to implement a Fast Floor function in Brightscript
		color=ff(color*255)
		color=abs(color)

    'make an rgba:
		ba=createobject("robytearray")
		ba.push(color)
		ba.push(color)
		ba.push(color)
		ba.push(255)
		hx=ba.tohexstring()
		
		color=HInt(ba.tohexstring())
		
		'no single pixel drawing method in brighscript so draw a rect
		screen.drawrect(x,y,1,1,color)
		screen.finish()		
		
	end for
end for
?"ok, now finish"

?"ok, now sleep"
while true
'not really sleeping, just looping
end while

end sub

function modulus(a,b) 
result1=a/b
return a-(int(a/b))*b
end function


Function HInt(hex_in as string) as integer
    out=0
    hex=LCase(hex_in)

    for i=1 to len(hex)
        dec1=asc(mid(hex, i, 1))
        if dec1>=asc("0") AND dec1 <=asc("9") then dec1=dec1-asc("0") else dec1=dec1-asc("a")+10
        out=out*16+dec1
    end for

    return out
end function 
