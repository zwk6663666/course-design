    program Problem2

    implicit none
    real DH, b, h, R1, R2, CKH, CKZ, A(11), IX(11), IZ(11), height(11), bg(11), hg(11), circleRectangle, AcircleRectangle, Arectangle, rectangle, Acircle, circle
    integer nshape, i
	open(1, file="in.txt")
	! print*, "请输入截面类型："
	! print*, '矩形：1                圆端形：2'
	! print*, '圆形：3                空心圆形：4'
	! print*, '退出：其他'
	read(1,*), nshape
	if(nshape == 1) then
		! print*, "请输入矩形的宽b，高h，桥墩墩台高DH，纵向坡度CKH，横向坡度CKZ"
		read(1,*), b, h, DH, CKH, CKZ
		call sliceSection(DH, CKH, CKZ, b, h, height, bg, hg, 1)
		do i = 1, 11
			A(i) = Arectangle(bg(i), hg(i))
			IX(i) = rectangle(bg(i), hg(i))
			IZ(i) = rectangle(hg(i), bg(i))
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	else if(nshape == 2) then
		! print*, "请输入圆端形的宽b，高h，桥墩墩台高DH，纵向坡度CKH"
		read(1,*), b, h, DH, CKH
		call sliceSection(DH, CKH, CKH, b, h, height, bg, hg, 2)
		do i = 1, 11
			A(i) = AcircleRectangle(bg(i), hg(i))
			IX(i) = circleRectangle(bg(i), hg(i), 1)
			IZ(i) = circleRectangle(hg(i), bg(i), 2)
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	else if(nshape == 3) then
		! print*, "请输入圆形的直径b，桥墩墩台高DH，纵向坡度CKH"
		read(1,*), b, DH, CKH
		call sliceSection(DH, CKH, CKH, b, b, height, bg, hg, 1)
		do i = 1, 11
			A(i) = Acircle(bg(i), 0.0)
			IX(i) = circle(bg(i), 0.0)
			IZ(i) = circle(bg(i), 0.0)
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	else if(nshape == 4) then
		! print*, "请输入空心环形的外直径d1，内直径d2，桥墩墩台高DH，纵向坡度CKH"
		read(1,*), b, h, DH, CKH
		call sliceSection(DH, CKH, CKH, b, h, height, bg, hg, 1)
		do i = 1, 11
			A(i) = Acircle(bg(i), hg(i))
			IX(i) = circle(bg(i), hg(i))
			IZ(i) = circle(bg(i), hg(i))
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	endif
	close(1)
    end program Problem2

	! 截面划分
	subroutine sliceSection(DH, CKH, CKZ, b, h, height, bg, hg, type)
		real DH, CKH, CKZ, height(11), bg(11), hg(11), b, h, slope
		integer i, type
		do i = 1, 11
			height(i) = (11 - i)*DH/10
			if(type == 1) then
				bg(i) = b + slope(CKH, (i - 1)*DH/10)
			else
				bg(i) = b	
			endif
			hg(i) = h + slope(CKZ, (i - 1)*DH/10)
		enddo
	end subroutine sliceSection
	! 结果打印
	subroutine printResult(IX, IZ, A, height, bg, hg)
		real IX(11), IZ(11), A(11), height(11), bg(11), hg(11)
		integer i
		open(2, file="out.txt")
		write(2,10), "截面序号","高度","截面宽","截面高","截面面积","IX","IZ"
		do i = 1, 11
			write(2,20) i, height(i), bg(i), hg(i), A(i), IX(i), IZ(i)
		enddo
		close(2)
10	format(A15, A15, A15, A15, A15, A15, A15)
20	format(I15, 6F15.4)
	end subroutine printResult
    
    !惯性矩
    !圆环形
    real function circle(d1, d2)
        real d1, d2
        circle = 3.1415923/64*(d1**4 - d2**4)
    end function circle
	! 半圆
	real function IhalfCircle(d, x)
		real d, x
		IhalfCircle = (3.141593*d**4)/128 - (2*d/(3*3.141593))**2*(3.141593*d**2)/8 + (x**2)*3.141593*d**2/8
	end function IhalfCircle
    !矩形
    real function rectangle(b, h)
        real b,h
        rectangle = (b*h**3) / 12.0
    end function rectangle
    !圆端形
    real function circleRectangle(b, h, type)
        real b, h, rectangle, IhalfCircle
		integer type
		if(type == 1) circleRectangle = rectangle(b, h) + 2*IhalfCircle(h, b/2 + 2*h/(3*3.141593))
		if(type == 2) circleRectangle = rectangle(h, b) + 2*IhalfCircle(h, 0.0)
    end function circleRectangle
    
    !面积
    !圆环形
    real function Acircle(d1, d2)
        real d1,d2
        Acircle = (3.1415923*(d1**2 - d2**2))/4
    end function Acircle
    !矩形
    real function Arectangle(b, h)
        real b, h
        Arectangle = b*h
    end function Arectangle
    !圆端形
    real function AcircleRectangle(b, h)
        real b, h, R
        AcircleRectangle = Acircle(h, 0.0) + Arectangle(b, h)
    end function AcircleRectangle
    
    !坡度计算
    real function slope(CK, l)
        real CK, l
        slope = ck*l
    end function slope
    
        
        