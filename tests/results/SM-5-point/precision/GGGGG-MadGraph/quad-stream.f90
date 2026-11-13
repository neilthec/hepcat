program hepcat_madgraph_quad_replay
 use iso_fortran_env, only: real64
 implicit none
 integer,parameter::rk=selected_real_kind(33)
 real(real64)::p53(0:3,5)
 real(rk)::p(0:3,5),answer
 real(rk),external::MATRIX
 integer::u,mask,leg,nhel(5),incoming(5),color(5),point_index
 character(len=2048)::pointfile
 call get_command_argument(1,pointfile)
 call setParamLog(.false.)
 call setpara('/private/tmp/hepcat-five-gluon-replay/reference-113/process/Cards/param_card.dat')
 open(newunit=u,file=trim(pointfile),form='unformatted',access='stream',status='old')
 do point_index=1,1000
 read(u)p53
 p=real(p53,kind=rk)
 if(point_index==1)print *, 'CORE_PRECISION_BITS',digits(answer),'CORE_STORAGE_BITS',storage_size(answer),'INPUT_STORAGE_BITS',storage_size(p53(0,1))
 color=1
 do mask=0,31
  do leg=1,5
   incoming(leg)=merge(2,-2,btest(mask,5-leg))
   nhel(leg)=incoming(leg)/2
   if(leg>2)nhel(leg)=-nhel(leg)
  end do
  answer=MATRIX(p,nhel,color)
  write(*,'(I5,1X,5(I3,1X),ES55.42E4)')point_index,incoming,answer
 end do
 end do
 close(u)
end program
