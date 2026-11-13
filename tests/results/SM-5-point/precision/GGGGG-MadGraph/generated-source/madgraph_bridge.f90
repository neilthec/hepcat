! Independent MadGraph reference; raw fixed-helicity squared matrix element.
integer(c_int) function hepcat_mg_initialize() bind(C, name="hepcat_mg_initialize")
  use iso_c_binding
  implicit none
  call setParamLog(.false.)
  call setpara('/private/tmp/hepcat-five-gluon-replay/reference-53-retry-1/p' // &
    'rocess/Cards/param_card.dat')
  hepcat_mg_initialize = 0
end function

subroutine hepcat_mg_evaluate(p, helicities, answer) bind(C, name="hepcat_mg_evaluate")
  use iso_c_binding
  implicit none
  real(c_double), intent(in) :: p(4,5)
  integer(c_int), intent(in) :: helicities(5)
  real(c_double), intent(out) :: answer
  integer :: color(5)
  double precision, external :: MATRIX
  color = 1
  answer = MATRIX(p, helicities, color)
end subroutine

! Raw exhaustive spin/color sum. No pruning, sampling, average or factorial.
subroutine hepcat_mg_evaluate_sum(p, answer) bind(C, name="hepcat_mg_evaluate_sum")
  use iso_c_binding
  implicit none
  real(c_double), intent(in) :: p(4,5)
  real(c_double), intent(out) :: answer
  integer(c_int) :: color(5)
  integer(c_int), save :: helicities(5,32)
  integer :: leg, state
  double precision, external :: MATRIX
  data (helicities(leg,1),leg=1,5) / &
    -1,-1,-1,-1,-1 /
  data (helicities(leg,2),leg=1,5) / &
    -1,-1,-1,-1,1 /
  data (helicities(leg,3),leg=1,5) / &
    -1,-1,-1,1,-1 /
  data (helicities(leg,4),leg=1,5) / &
    -1,-1,-1,1,1 /
  data (helicities(leg,5),leg=1,5) / &
    -1,-1,1,-1,-1 /
  data (helicities(leg,6),leg=1,5) / &
    -1,-1,1,-1,1 /
  data (helicities(leg,7),leg=1,5) / &
    -1,-1,1,1,-1 /
  data (helicities(leg,8),leg=1,5) / &
    -1,-1,1,1,1 /
  data (helicities(leg,9),leg=1,5) / &
    -1,1,-1,-1,-1 /
  data (helicities(leg,10),leg=1,5) / &
    -1,1,-1,-1,1 /
  data (helicities(leg,11),leg=1,5) / &
    -1,1,-1,1,-1 /
  data (helicities(leg,12),leg=1,5) / &
    -1,1,-1,1,1 /
  data (helicities(leg,13),leg=1,5) / &
    -1,1,1,-1,-1 /
  data (helicities(leg,14),leg=1,5) / &
    -1,1,1,-1,1 /
  data (helicities(leg,15),leg=1,5) / &
    -1,1,1,1,-1 /
  data (helicities(leg,16),leg=1,5) / &
    -1,1,1,1,1 /
  data (helicities(leg,17),leg=1,5) / &
    1,-1,-1,-1,-1 /
  data (helicities(leg,18),leg=1,5) / &
    1,-1,-1,-1,1 /
  data (helicities(leg,19),leg=1,5) / &
    1,-1,-1,1,-1 /
  data (helicities(leg,20),leg=1,5) / &
    1,-1,-1,1,1 /
  data (helicities(leg,21),leg=1,5) / &
    1,-1,1,-1,-1 /
  data (helicities(leg,22),leg=1,5) / &
    1,-1,1,-1,1 /
  data (helicities(leg,23),leg=1,5) / &
    1,-1,1,1,-1 /
  data (helicities(leg,24),leg=1,5) / &
    1,-1,1,1,1 /
  data (helicities(leg,25),leg=1,5) / &
    1,1,-1,-1,-1 /
  data (helicities(leg,26),leg=1,5) / &
    1,1,-1,-1,1 /
  data (helicities(leg,27),leg=1,5) / &
    1,1,-1,1,-1 /
  data (helicities(leg,28),leg=1,5) / &
    1,1,-1,1,1 /
  data (helicities(leg,29),leg=1,5) / &
    1,1,1,-1,-1 /
  data (helicities(leg,30),leg=1,5) / &
    1,1,1,-1,1 /
  data (helicities(leg,31),leg=1,5) / &
    1,1,1,1,-1 /
  data (helicities(leg,32),leg=1,5) / &
    1,1,1,1,1 /
  color = 1
  answer = 0d0
  do state = 1, 32
    answer = answer + MATRIX(p, helicities(:,state), color)
  enddo
end subroutine
