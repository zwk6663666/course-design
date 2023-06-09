    program Problem5

    implicit none

    real b, h, area, Igroup(2), position(2), Irectangle, areaRect, r1, r2, areaCircle, Icircle, areaIshape,&
        bs, hs, bx, hx, hf, b1, centroid, Ishape, d, h1, bf, areaCircleBoard, areaRectBoard
	integer i, j, nshape, n
	do
		print*, "请输入截面类型："
		print*, "矩形：1        圆（环）形：2        工字形：3"
		print*, "圆（端）形空心板：4              矩形空心板：5"
		print*, "退出：其他数字"
		read*, nshape
		if(nshape == 1) then
			print*, "请输入矩形的宽b，高h："
			read*, b, h
			area = areaRect(b, h)
			Igroup(1) = Irectangle(b, h, 0.0)
			Igroup(2) = Irectangle(h, b, 0.0)
			position(1) = b/2.0
			position(2) = h/2.0
			call printResult(area, Igroup, position)
		else if(nshape == 2) then
			print*, "请输入圆（环）形的外圆R1，内圆R2："
			read*, r1, r2
			area = areaCircle(r1, r2)
			Igroup = Icircle(r1, r2, 0.0)
			position = r1/2.0
			call printResult(area, Igroup, position)
		else if(nshape == 3) then
			print*, "请输入工字形截面的上翼板宽bs，厚hs，下翼板宽bx，厚hx，腹板高hf，厚b1："
			read*, bs, hs, bx, hx, hf, b1
			area = areaIshape(bs, hs, bx, hx, hf, b1)
			position(1) = bs/2.0
			if(bs < bx) position(1) = bx/2.0
			position(2) = centroid(bs, hs, bx, hx, hf, b1)
			call IIshape(bs, hs, bx, hx, hf, b1, position(2), Igroup)
			call printResult(area, Igroup, position)
		else if(nshape == 4) then
			print*, "请输入圆（端）形空心板的宽b，高h，圆直径d，矩形高h1，总间隔厚度bf, 空洞数n："
			read*, b, h, d, h1, bf, n
			area = areaCircleBoard(b, h, d, h1, n)
			position(1) = b/2.0
			position(2) = h/2.0
			call IcircleBoard(Igroup, b, h, d, bf, h1, n)
			call printResult(area, Igroup, position)
		else if(nshape == 5) then
			print*, "请输入矩形空心板的宽b，高h，空洞宽b1，高h1，总间隔厚度bf, 空洞数n："
			read*, b, h, b1, h1, bf, n
			area = areaRectBoard(b, h, b1, h1, n)
			position(1) = b/2.0
			position(2) = h/2.0
			call IrectBoard(Igroup, b, h, b1, h1, bf, n)
			call printResult(area, Igroup, position)
        else
            exit
		endif
	enddo

    end program Problem5

    !打印
    subroutine printResult(area, Igroup, position)
		real area, Igroup(2), position(2)
		print*, "------------------------------------------------------------------------------"
		print 10, "面积", "x轴惯性矩", "y轴惯性矩", "形心位置"
		print 20, area, Igroup(1), Igroup(2), "(", position(1),",",position(2),")"
		print*, "-------------------------------------------------------------------------------"
10  format(A16, A16, A16, A16)
20	format(3F16.4, 4x, A1, F7.2, A1, F7.2, A1)
	end subroutine printResult
	! 截面面积
	! 圆形
	real function areaCircle(r1, r2)
		real r1, r2
		areaCircle = 3.14159*(r1**2 - r2**2)
	end function areaCircle
	!矩形
	real function areaRect(b, h)
		real b, h
		areaRect = b*h 
	end function areaRect
	! 工字形
	real function areaIshape(bs, hs, bx, hx, hf, b1)
		real bs, hs, bx, hx, hf, b1
		areaIshape = bs*hs + bx*hx + b1*hf
	end function areaIshape

	! 圆空心板
	real function areaCircleBoard(b, h, d, h1, n)
		integer n
		real b, h, d, h1, areaCircle, areaRect
		areaCircleBoard = areaRect(b,h) - n*(areaCircle(d/2.0, 0.0) + areaRect(d, h1))
	end function areaCircleBoard
	! 矩形空心板
	real function areaRectBoard(b, h, b1, h1, n)
		integer n
		real b, h, b1, h1
		areaRectBoard = areaRect(b, h) - n*areaRect(b1, h1)
	end function areaRectBoard
    !形心位置
    real function centroid(bs, hs, bx, hx, hf, b1)
        integer i
        real group(3, 2), s, areaRect, areaIshape
		group(1, 1) = areaRect(bs, hs)
		group(2, 1) = areaRect(b1, hf)
		group(3, 1) = areaRect(bx, hx)
		group(1, 2) = hs/2.0
		group(2, 2) = hs + hf/2.0
		group(3, 2) = hs + hf + hx/2.0
        s = 0.0
        do i = 1, 3
            s = s + group(i, 1)*group(i, 2)
        enddo
        centroid = s / areaIshape(bs, hs, bx, hx, hf, b1)
    end function centroid
    
    !惯性矩
    !矩形
    real function Irectangle(b, h, x)
        real b, h, x
        Irectangle = b*h**3/12.0 + b*h*x**2
    end function Irectangle
    !工字型
    subroutine IIshape(bs, hs, bx, hx, hf, b1, y, Igroup)
        real Igroup(2), bs, bx, b1, hs, hx, hf, y, Irectangle
        ! 竖直
        Igroup(1) = Irectangle(hs, bs, 0.0) + Irectangle(hf, b1, 0.0) + Irectangle(hx, bx, 0.0)
        ! 水平
        Igroup(2) = Irectangle(bs, hs, y-hs/2.0) + Irectangle(b1, hf, 0.0) + Irectangle(bx, hx, hs + hf + hx - y - hx / 2.0)
    end subroutine IIshape
    ! 圆形
    real function Icircle(r1, r2, x)
		real r1, r2, x
        Icircle = 3.141593 * (r1**4 - r2**4) / 64.0 + areaCircle(r1, r2)*x**2
	end function Icircle
	! 半圆
	real function IhalfCircle(d, x)
		real d, x
		IhalfCircle = (3.141593*d**4)/128 - (2*d/(3*3.141593))**2*(3.141593*d**2)/8 + (x**2)*3.141593*d**2/8
	end function IhalfCircle
	! 圆空心板
	subroutine IcircleBoard(I, b, h, d, bf, h1, n)
		integer n, j, k
		real b, h, d, bf, h1, I(2), Irectangle, IhalfCircle
		! 水平
		I(1) = Irectangle(b, h, 0.0) - 2*n*IhalfCircle(d, 2*d/(3*3.141593) + h1/2.0) - n*Irectangle(d, h1, 0.0)
		! 竖直
		I(2) = Irectangle(h, b, 0.0)
		if(mod(n, 2) == 0) then
			do j = 1, n/2
                print*, Irectangle(h1, d, (j - 0.5)*bf/(n-1) + (j - 0.5)*d)
				print*, IhalfCircle(d, (j - 0.5)*bf/(n-1) + (j - 0.5)*d)
				I(2) = I(2) - 2*(Irectangle(h1, d, (j - 0.5)*bf/(n-1) + (j - 0.5)*d) + 2*IhalfCircle(d, (j - 0.5)*bf/(n-1) + (j - 0.5)*d))
			enddo
		else
			I(2) = I(2) - Irectangle(h1, d, 0.0) - IhalfCircle(d, 0.0)
			do j = 1, (n - 1)/2
				I(2) = I(2) - 2*(Irectangle(h1, d, j*bf/(n-1) + j*d) + 2*IhalfCircle(d, j*bf/(n-1) + j*d))
			enddo
		endif
		
	end subroutine ICircleBoard
	! 矩形空心板
	subroutine IrectBoard(I, b, h, b1, h1, bf, n)
		integer n, j, k
		real b, h, b1, h1, bf, I(2), Irectangle
		! 水平
		I(1) = Irectangle(b, h, 0.0) -  n*Irectangle(b1, h1, 0.0)
		! 竖直
		I(2) = Irectangle(h, b, 0.0)
		if(mod(n, 2) == 0) then
			do j = 1, n/2
				I(2) = I(2) - 2*Irectangle(h1, b1, (j - 0.5)*bf/(n-1) + (j - 0.5)*b1)
			enddo
		else
			I(2) = I(2) - Irectangle(h1, b1, 0.0)
			do j = 1, (n - 1)/2
        		I(2) = I(2) - 2*Irectangle(h1, b1, j*bf/(n-1) + j*b1)
      		enddo
		endif
    end subroutine IrectBoard