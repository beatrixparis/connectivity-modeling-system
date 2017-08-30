MODULE mod_TempMort

 USE mod_kinds

 IMPLICIT NONE

 CONTAINS


!**************************************************************

 SUBROUTINE define_halflife(temp, halflife)

 real (kind = real_kind), intent(in)    :: temp
 real (kind = real_kind), intent(out)   :: halflife

 integer (kind=int_kind)                :: i
 integer (kind=int_kind), dimension(16) :: temp_ranges
 real (kind=real_kind), dimension(15)   :: halflifes

!define temperature ranges
 temp_ranges = (/(i, i=8,38,2)/)

!define halflife for each temperature range
 halflifes = (/3283200, 3110400, 2937600, 2764800, 2592000, 2419200, 2246400, 2073600, 1900800, 1728000, 1555200, 1382400, 1209600, 1036800, 864000/)

!determine halflife for inputted temperature
 IF(temp > maxval(temp_ranges) .or. temp < minval(temp_ranges)) THEN
   print*, 'Warning: Temperature is outside defined range for mod_envmort:', minval(temp_ranges), ' to', maxval(temp_ranges) 
 ENDIF
   DO i = 1, 15 !loop over length of temp_ranges - 1
     IF(temp >= temp_ranges(i) .and. temp < temp_ranges(i+1)) THEN
        halflife = halflifes(i)
     ENDIF
   ENDDO

 END SUBROUTINE define_halflife

!**************************************************************


END MODULE mod_TempMort
