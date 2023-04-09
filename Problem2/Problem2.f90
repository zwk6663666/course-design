    program Problem2

    implicit none
    real DH, b, h, R1, R2, CKH, CKZ, A(11), IX(11), IZ(11), height(11), bg(11), hg(11), circleRectangle, AcircleRectangle, Arectangle, rectangle, Acircle, circle
    integer nshape, i
	open(1, file="in.txt")
	! print*, "������������ͣ�"
	! print*, '���Σ�1                Բ���Σ�2'
	! print*, 'Բ�Σ�3                ����Բ�Σ�4'
	! print*, '�˳�������'
	read(1,*), nshape
	if(nshape == 1) then
		! print*, "��������εĿ�b����h���Ŷն�̨��DH�������¶�CKH�������¶�CKZ"
		read(1,*), b, h, DH, CKH, CKZ
		call sliceSection(DH, CKH, CKZ, b, h, height, bg, hg, 1)
		do i = 1, 11
			A(i) = Arectangle(bg(i), hg(i))
			IX(i) = rectangle(bg(i), hg(i))
			IZ(i) = rectangle(hg(i), bg(i))
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	else if(nshape == 2) then
		! print*, "������Բ���εĿ�b����h���Ŷն�̨��DH�������¶�CKH"
		read(1,*), b, h, DH, CKH
		call sliceSection(DH, CKH, CKH, b, h, height, bg, hg, 2)
		do i = 1, 11
			A(i) = AcircleRectangle(bg(i), hg(i))
			IX(i) = circleRectangle(bg(i), hg(i), 1)
			IZ(i) = circleRectangle(hg(i), bg(i), 2)
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	else if(nshape == 3) then
		! print*, "������Բ�ε�ֱ��b���Ŷն�̨��DH�������¶�CKH"
		read(1,*), b, DH, CKH
		call sliceSection(DH, CKH, CKH, b, b, height, bg, hg, 1)
		do i = 1, 11
			A(i) = Acircle(bg(i), 0.0)
			IX(i) = circle(bg(i), 0.0)
			IZ(i) = circle(bg(i), 0.0)
		enddo
		call printResult(IX, IZ, A, height, bg, hg)
	else if(nshape == 4) then
		! print*, "��������Ļ��ε���ֱ��d1����ֱ��d2���Ŷն�̨��DH�������¶�CKH"
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

	! ���滮��
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
	! �����ӡ
	subroutine printResult(IX, IZ, A, height, bg, hg)
		real IX(11), IZ(11), A(11), height(11), bg(11), hg(11)
		integer i
		open(2, file="out.txt")
		write(2,10), "�������","�߶�","�����","�����","�������","IX","IZ"
		do i = 1, 11
			write(2,20) i, height(i), bg(i), hg(i), A(i), IX(i), IZ(i)
		enddo
		close(2)
10	format(A15, A15, A15, A15, A15, A15, A15)
20	format(I15, 6F15.4)
	end subroutine printResult
    
    !���Ծ�
    !Բ����
    real function circle(d1, d2)
        real d1, d2
        circle = 3.1415923/64*(d1**4 - d2**4)
    end function circle
	! ��Բ
	real function IhalfCircle(d, x)
		real d, x
		IhalfCircle = (3.141593*d**4)/128 - (2*d/(3*3.141593))**2*(3.141593*d**2)/8 + (x**2)*3.141593*d**2/8
	end function IhalfCircle
    !����
    real function rectangle(b, h)
        real b,h
        rectangle = (b*h**3) / 12.0
    end function rectangle
    !Բ����
    real function circleRectangle(b, h, type)
        real b, h, rectangle, IhalfCircle
		integer type
		if(type == 1) circleRectangle = rectangle(b, h) + 2*IhalfCircle(h, b/2 + 2*h/(3*3.141593))
		if(type == 2) circleRectangle = rectangle(h, b) + 2*IhalfCircle(h, 0.0)
    end function circleRectangle
    
    !���
    !Բ����
    real function Acircle(d1, d2)
        real d1,d2
        Acircle = (3.1415923*(d1**2 - d2**2))/4
    end function Acircle
    !����
    real function Arectangle(b, h)
        real b, h
        Arectangle = b*h
    end function Arectangle
    !Բ����
    real function AcircleRectangle(b, h)
        real b, h, R
        AcircleRectangle = Acircle(h, 0.0) + Arectangle(b, h)
    end function AcircleRectangle
    
    !�¶ȼ���
    real function slope(CK, l)
        real CK, l
        slope = ck*l
    end function slope
    
        
        