    program Problem5

    implicit none

    real b, h, area, Igroup(2), position(2), Irectangle, areaRect, r1, r2, areaCircle, Icircle, areaIshape,&
        bs, hs, bx, hx, hf, b1, centroid, Ishape, d, h1, bf, areaCircleBoard, areaRectBoard
	integer i, j, nshape, n
	do
		print*, "������������ͣ�"
		print*, "���Σ�1        Բ�������Σ�2        �����Σ�3"
		print*, "Բ���ˣ��ο��İ壺4              ���ο��İ壺5"
		print*, "�˳�����������"
		read*, nshape
		if(nshape == 1) then
			print*, "��������εĿ�b����h��"
			read*, b, h
			area = areaRect(b, h)
			Igroup(1) = Irectangle(b, h, 0.0)
			Igroup(2) = Irectangle(h, b, 0.0)
			position(1) = b/2.0
			position(2) = h/2.0
			call printResult(area, Igroup, position)
		else if(nshape == 2) then
			print*, "������Բ�������ε���ԲR1����ԲR2��"
			read*, r1, r2
			area = areaCircle(r1, r2)
			Igroup = Icircle(r1, r2, 0.0)
			position = r1/2.0
			call printResult(area, Igroup, position)
		else if(nshape == 3) then
			print*, "�����빤���ν�����������bs����hs���������bx����hx�������hf����b1��"
			read*, bs, hs, bx, hx, hf, b1
			area = areaIshape(bs, hs, bx, hx, hf, b1)
			position(1) = bs/2.0
			if(bs < bx) position(1) = bx/2.0
			position(2) = centroid(bs, hs, bx, hx, hf, b1)
			call IIshape(bs, hs, bx, hx, hf, b1, position(2), Igroup)
			call printResult(area, Igroup, position)
		else if(nshape == 4) then
			print*, "������Բ���ˣ��ο��İ�Ŀ�b����h��Բֱ��d�����θ�h1���ܼ�����bf, �ն���n��"
			read*, b, h, d, h1, bf, n
			area = areaCircleBoard(b, h, d, h1, n)
			position(1) = b/2.0
			position(2) = h/2.0
			call IcircleBoard(Igroup, b, h, d, bf, h1, n)
			call printResult(area, Igroup, position)
		else if(nshape == 5) then
			print*, "��������ο��İ�Ŀ�b����h���ն���b1����h1���ܼ�����bf, �ն���n��"
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

    !��ӡ
    subroutine printResult(area, Igroup, position)
		real area, Igroup(2), position(2)
		print*, "------------------------------------------------------------------------------"
		print 10, "���", "x����Ծ�", "y����Ծ�", "����λ��"
		print 20, area, Igroup(1), Igroup(2), "(", position(1),",",position(2),")"
		print*, "-------------------------------------------------------------------------------"
10  format(A16, A16, A16, A16)
20	format(3F16.4, 4x, A1, F7.2, A1, F7.2, A1)
	end subroutine printResult
	! �������
	! Բ��
	real function areaCircle(r1, r2)
		real r1, r2
		areaCircle = 3.14159*(r1**2 - r2**2)
	end function areaCircle
	!����
	real function areaRect(b, h)
		real b, h
		areaRect = b*h 
	end function areaRect
	! ������
	real function areaIshape(bs, hs, bx, hx, hf, b1)
		real bs, hs, bx, hx, hf, b1
		areaIshape = bs*hs + bx*hx + b1*hf
	end function areaIshape

	! Բ���İ�
	real function areaCircleBoard(b, h, d, h1, n)
		integer n
		real b, h, d, h1, areaCircle, areaRect
		areaCircleBoard = areaRect(b,h) - n*(areaCircle(d/2.0, 0.0) + areaRect(d, h1))
	end function areaCircleBoard
	! ���ο��İ�
	real function areaRectBoard(b, h, b1, h1, n)
		integer n
		real b, h, b1, h1
		areaRectBoard = areaRect(b, h) - n*areaRect(b1, h1)
	end function areaRectBoard
    !����λ��
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
    
    !���Ծ�
    !����
    real function Irectangle(b, h, x)
        real b, h, x
        Irectangle = b*h**3/12.0 + b*h*x**2
    end function Irectangle
    !������
    subroutine IIshape(bs, hs, bx, hx, hf, b1, y, Igroup)
        real Igroup(2), bs, bx, b1, hs, hx, hf, y, Irectangle
        ! ��ֱ
        Igroup(1) = Irectangle(hs, bs, 0.0) + Irectangle(hf, b1, 0.0) + Irectangle(hx, bx, 0.0)
        ! ˮƽ
        Igroup(2) = Irectangle(bs, hs, y-hs/2.0) + Irectangle(b1, hf, 0.0) + Irectangle(bx, hx, hs + hf + hx - y - hx / 2.0)
    end subroutine IIshape
    ! Բ��
    real function Icircle(r1, r2, x)
		real r1, r2, x
        Icircle = 3.141593 * (r1**4 - r2**4) / 64.0 + areaCircle(r1, r2)*x**2
	end function Icircle
	! ��Բ
	real function IhalfCircle(d, x)
		real d, x
		IhalfCircle = (3.141593*d**4)/128 - (2*d/(3*3.141593))**2*(3.141593*d**2)/8 + (x**2)*3.141593*d**2/8
	end function IhalfCircle
	! Բ���İ�
	subroutine IcircleBoard(I, b, h, d, bf, h1, n)
		integer n, j, k
		real b, h, d, bf, h1, I(2), Irectangle, IhalfCircle
		! ˮƽ
		I(1) = Irectangle(b, h, 0.0) - 2*n*IhalfCircle(d, 2*d/(3*3.141593) + h1/2.0) - n*Irectangle(d, h1, 0.0)
		! ��ֱ
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
	! ���ο��İ�
	subroutine IrectBoard(I, b, h, b1, h1, bf, n)
		integer n, j, k
		real b, h, b1, h1, bf, I(2), Irectangle
		! ˮƽ
		I(1) = Irectangle(b, h, 0.0) -  n*Irectangle(b1, h1, 0.0)
		! ��ֱ
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