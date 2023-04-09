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
    !数据读入
    do i = 1, n
        read(1,*) classes(i)%id, classes(i)%a
        s = 0.0
        do j = 1, num
            s = s + classes(i)%a(j)
        enddo
        classes(i)%sum = s
    enddo
    close(1)
    !各门课程平局分计算 average记录
    do i = 1, num
        s = 0.0
        do j = 1, n
            s = s + classes(j)%a(i)
        enddo
        average(i) = s / n
    enddo
    !统计各门课高于和低于平均值的人数
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
    print*, '每科低于平均值人数：'
    write(*, 60), '科目1',' 科目2', '科目3', '科目4', '科目5'
    write(*, 10), low
    print*, ''
    print*, '每科高于平均值人数：'
    write(*, 60), '科目1',' 科目2', '科目3', '科目4', '科目5'
    write(*, 10), high
    !总分排序(冒泡）
    !do i = 1, n - 1
    !    do j = 1, n - i
    !        if(classes(j)%sum < classes(j + 1)%sum) then
    !            t = classes(j)%sum
    !            classes(j)%sum = classes(j + 1)%sum
    !            classes(j + 1)%sum = t
    !        endif
    !    enddo
    !enddo
    
    !总分排序（简单插入）
    !do i = 1, n - 1
    !    do j = i + 1, n
    !        if(classes(j)%sum < classes(j)%sum) then
    !            t = classes(j)%sum
    !            classes(i)%sum = classes(j)%sum
    !            classes(j)%sum = t
    !        endif
    !    enddo
    !enddo
    !总分排序（选择排序）
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
    write(2, 30) '名次', '学号', '总分'
    do i = 1, n
        write(2, 20) i, classes(i)%id, classes(i)%sum
    enddo
    print*, ''
    print*, '名次信息已写入studentOUT,txt文件'
    print*, ''
    !分数查询
    do
        print*, '请输入学号，查询成绩（输入111退出）：'
        read*, enterId
        flag = 0
        if( enterId == 111) exit
        do i = 1, n
            if(enterId == classes(i)%id) then
                write(*, 40) '名次', '科目1',' 科目2', '科目3', '科目4', '科目5', '总分'
                write(*, 50) i, classes(i)%a, classes(i)%sum
                flag = -1
                exit
            endif
        enddo
        if(flag == 0) print*, '请重新输入学号，无此人记录，查询失败'
    enddo
10  format (5I8)    
20  format (I6, I15, F15.2)  
30  format (A6, A15, A15)   
40  format (A4, 6A13)
50  format (I4, 5F13.2, F13.2)
60  format (5A8)    
    end program Problem3
