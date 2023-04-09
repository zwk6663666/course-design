    program Problem4

    implicit none
    real d1, d2, d3, d4, length(4,8), delta(4,4), EI(4), distance(4,4), figureMult
    integer i, j
    delta = 0.0
    print*,"请输入d1, d2, d3, d4的值："
    read*, d1, d2, d3, d4
    EI = (/2.7E10*10, 2.7E10*15, 2.7E10*20, 2.7E10*25/)
    !距离矩阵
    length = reshape((/0.0, d1, d1+d2, d1+d2+d3, d1, d1+d2, d1+d2+d3, d1+d2+d3+d4,&
        0.0, 0.0, d2, d2+d3, 0.0, d2, d3+d4, d2+d3+d4,&
        0.0, 0.0, 0.0, d3, 0.0, 0.0, d3, d3+d4,&
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, d4/), shape(length))
    !数据代入
    do i = 1, 4
        do j = 1, 4
            distance(:,1:2) = length(:, 2*i-1:2*i)
            distance(:,3:4) = length(:, 2*j-1:2*j)
            delta(i,j) = figureMult(distance, EI)
        enddo
    enddo
    !数据打印
    print*, '---------------------------------------------------------------'
    do i = 1, 4
        print*, delta(i,:)
    enddo
    end program Problem4

    !图乘函数
    real function figureMult(distance, EI)
        real distance(4,4), EI(4)
        integer i
        figureMult = 0.0
        do i = 1, 4
            figureMult = figureMult + 1.0/(6*EI(i))*(2*distance(i, 1)*distance(i, 3) + 2*distance(i, 2)*distance(i, 4) + distance(i, 1)*distance(i, 4) + distance(i, 2)*distance(i, 3))
        enddo
        
    end function figureMult