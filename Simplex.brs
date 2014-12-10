'3D simplex noise
function noise( xin#,  yin#,  zin#) as  double
	'Skew the input space to determine which simplex cell we're in
F3# = 1.0/3.0
 
s# = (xin#+yin#+zin#)*F3#
	'Very nice and simple skew factor for 3D 

i% = m.fastfloor(xin#+s#)
j% = m.fastfloor(yin#+s#)
k% = m.fastfloor(zin#+s#)



 
g3# = 1.0/6.0 
	'Very nice and simple unskew factor, too 
	
t# = (i%+j%+k%)*G3#


X0# = i%-t#
	'Unskew the cell origin back to (x,y,z) space 

Y0# = j%-t#

Z0# = k%-t#
x0# = xin#-X0#

	'The x,y,z distances from the cell origin 
	
y0# = yin#-Y0#
z0# = zin#-Z0#

	'For the 3D case, the simplex shape is a slightly irregular tetrahedron. 
	'Determine which simplex we are in. 

	'Offsets for second corner of simplex in (i,j,k) coords 
	 
	'Offsets for third corner of simplex in (i,j,k) coords

if x0# >= y0#	
	if y0# >= z0# then
	'X Y Z order
		i1%=1
		j1%=0
		k1%=0
		i2%=1
		j2%=1
		k2%=0
	else if x0# >= z0# 
	'X Z Y order
		i1%=1
		j1%=0
		k1%=0
		i2%=1
		j2%=0
		k2%=1
	else
	'Z X Y order
		i1%=0
		j1%=0
		k1%=1
		i2%=1
		j2%=0
		k2%=1
	end if
else
	if y0# < z0# 
	'Z Y X order	
		i1%=0
		j1%=0
		k1%=1
		i2%=0
		j2%=1
		k2%=1   
	else if x0# < z0# then
	'Y Z X order
		i1%=0
		j1%=1
		k1%=0
		i2%=0
		j2%=1
k2%=1 
	else
	'Y X Z order
		i1%=0
		j1%=1
		k1%=0
		i2%=1
		j2%=1
		k2%=0  
	end if
end if
	

'A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z), 
'a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and 
'a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where // c = 1/6.

'Add contributions from each corner to get the final noise value. 
'The result is scaled to stay just inside [-1,1] return 32.0*(n0# + n1# + n2 + n3)




x1# = x0# - i1% + G3#

y1# = y0# - j1% + G3#
 
 z1# = z0# - k1% + G3#
 
 x2# = x0# - i2% + 2.0*G3#
 'Offsets for third corner in (x,y,z) coords 
 y2# = y0# - j2% + 2.0*G3#

 z2# = z0# - k2% + 2.0*G3#
 
 x3# = x0# - 1.0 + 3.0*G3#
 'Offsets for last corner in (x,y,z) coords 
 y3# = y0# - 1.0 + 3.0*G3#
 
 z3# = z0# - 1.0 + 3.0*G3#


'Work out the hashed gradient indices of the four simplex corners
ii% = i% and 255
jj% = j% and 255
kk% = k% and 255
gi0% = modulus(m.perm[ii%+m.perm[jj%+m.perm[kk%]]],12)
gi1% = modulus(m.perm[ii%+i1%+m.perm[jj%+j1%+m.perm[kk%+k1%]]],12)
gi2% = modulus(m.perm[ii%+i2%+m.perm[jj%+j2%+m.perm[kk%+k2%]]],12)
gi3% = modulus(m.perm[ii%+1+m.perm[jj%+1+m.perm[kk%+1]]],12)

'Calculate the contribution from the four corners
t0# = 0.5 - x0#*x0# - y0#*y0# - z0#*z0#

if t0# <0 then 
	n0# = 0.0
else 
	t0# = t0#*t0#
	n0# = t0# * t0# * m.dot(m.grad3[gi0%], x0#, y0#, z0#)
	
end if
t1# = 0.6 - x1#*x1# - y1#*y1# - z1#*z1#
if t1#<0 then 
	n1# = 0.0
else 
	t1# =t1#*t1#
	n1# = t1# * t1# * m.dot(m.grad3[gi1%], x1#, y1#, z1#)
end if
	t2# = 0.6 - x2#*x2# - y2#*y2# - z2#*z2#
	
if t2#<0 then 
	 n2# = 0.0
else 
	t2# = t2#*t2#
	n2# = t2# * t2# * m.dot(m.grad3[gi2%], x2#, y2#, z2#)
end if
t3# = 0.6 - x3#*x3# - y3#*y3# - z3#*z3#

if t3#<0 then 
n3# = 0.0
 else 
t3# = t3#*t3#
 n3# = t3# * t3# * m.dot(m.grad3[gi3%], x3#, y3#, z3#)
end if

'? n0#,n1#,n2#,n3#
'Add contributions from each corner to get the final noise value. 
'The result is scaled to stay just inside [-1,1] 
return 32.0*(n0# + n1# + n2# + n3#)

end function


