    program Problem3
    implicit none
    integer n, i, j, num, t, p, enterId, flag
    real s
    parameter (n = 10, num = 5)
    real average(num)
    integer high(num), low(num)
    
    type student_record
        integer id
        real a(5), sum
    end type
    
    type (student_record) classes(n)
    type (student_record) orderClass(n)
    open(1, file='studentIn.txt')
    !���ݶ���
    do i = 1, n
        read(1,*) classes(i)%id, classes(i)%a
        s = 0.0
        do j = 1, num
            s = s + classes(i)%a(j)
        enddo
        classes(i)%sum = s
    enddo
    close(1)
    !���ſγ�ƽ�ַּ��� average��¼
    do i = 1, num
        s = 0.0
        do j = 1, n
            s = s + classes(j)%a(i)
        enddo
        average(i) = s / n
    enddo
    !ͳ�Ƹ��ſθ��ں͵���ƽ��ֵ������
    do i = 1, num
        high(i) = 0
        low(i) = 0
        do j = 1, n
            if(average(i) <= classes(j)%a(i)) then
            high(i) = high(i) + 1
            else
                low(i) = low(i) + 1
            endif
        enddo
    enddo
    print*, 'ÿ�Ƶ���ƽ��ֵ������'
    write(*, 60), '��Ŀ1',' ��Ŀ2', '��Ŀ3', '��Ŀ4', '��Ŀ5'
    write(*, 10), low
    print*, ''
    print*, 'ÿ�Ƹ���ƽ��ֵ������'
    write(*, 60), '��Ŀ1',' ��Ŀ2', '��Ŀ3', '��Ŀ4', '��Ŀ5'
    write(*, 10), high
    !�ܷ�����(ð�ݣ�
    !do i = 1, n - 1
    !    do j = 1, n - i
    !        if(classes(j)%sum < classes(j + 1)%sum) then
    !            t = classes(j)%sum
    !            classes(j)%sum = classes(j + 1)%sum
    !            classes(j + 1)%sum = t
    !        endif
    !    enddo
    !enddo
    
    !�ܷ����򣨼򵥲��룩
    !do i = 1, n - 1
    !    do j = i + 1, n
    !        if(classes(j)%sum < classes(j)%sum) then
    !            t = classes(j)%sum
    !            classes(i)%sum = classes(j)%sum
    !            classes(j)%sum = t
    !        endif
    !    enddo
    !enddo
    !�ܷ�����ѡ������
    do i = 1, n - 1
        p = i
        do j = i + 1, n
            if(classes(j)%sum > classes(p)%sum) p = j
        enddo
            t = classes(i)%sum
            classes(i)%sum = classes(p)%sum
            classes(p)%sum = t
    enddo
    open(2, file='studentOut.txt')
    write(2, 30) '����', 'ѧ��', '�ܷ�'
    do i = 1, n
        write(2, 20) i, classes(i)%id, classes(i)%sum
    enddo
    print*, ''
    print*, '������Ϣ��д��studentOUT,txt�ļ�'
    print*, ''
    !������ѯ
    do
        print*, '������ѧ�ţ���ѯ�ɼ�������111�˳�����'
        read*, enterId
        flag = 0
        if( enterId == 111) exit
        do i = 1, n
            if(enterId == classes(i)%id) then
                write(*, 40) '����', '��Ŀ1',' ��Ŀ2', '��Ŀ3', '��Ŀ4', '��Ŀ5', '�ܷ�'
                write(*, 50) i, classes(i)%a, classes(i)%sum
                flag = -1
                exit
            endif
        enddo
        if(flag == 0) print*, '����������ѧ�ţ��޴��˼�¼����ѯʧ��'
    enddo
10  format (5I8)    
20  format (I6, I15, F15.2)  
30  format (A6, A15, A15)   
40  format (A4, 6A13)
50  format (I4, 5F13.2, F13.2)
60  format (5A8)    
    end program Problem3
