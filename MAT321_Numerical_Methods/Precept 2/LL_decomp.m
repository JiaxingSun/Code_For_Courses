function [Lest] = LL_decomp(A)
[m,n]=size(A);
Lest=zeros(m,m);
Row=1;
Column=1;
j=1;
for i=1:m
    tree=sqrt(A(1,1));
    Lest(Row,Column)=tree;
    if(m~=1)   
        L21=A(j+1:m,1)/tree;
        Lest(Row+1:end,Column)=L21;
        A=(A(j+1:m,j+1:m)-L21*L21');
        [m,n]=size(A);
        Row=Row+1;
        Column=Column+1;
    end
end