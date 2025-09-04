! Definitions of custom data types
module mod_typedefs
    use mod_decl_const, only : number_of_layers => ML
    use, intrinsic :: iso_c_binding, only: c_double, c_int
    implicit none

    type groundwater_variables
        integer :: vertical_flow_mode        ! 0 for seepage at lower boundary, 1 for considering groundwater in within the layers or beneath them
        real(kind=c_double) :: water_table_depth    ! depth of groundwater in m, negative for below surface groundwater, only affects calculations when vertical_flow_mode = 1
        integer :: N_groundwater            ! number of layer in which water table is located, should be calculated as NLAYER+1 if groundwater is below layers
        real(kind=c_double) :: dep(number_of_layers)             ! positions of midpoint of layers [m], negative below surface

        integer :: asdf                        ! Zähler für die Fehlersuche
    end type groundwater_variables

    contains

    function ve_fl_mo(gw_l)
    ! Set vertical_flow_mode depending on water_table_depth
    ! If water_table_depth subceeds or equals -999,
    ! vertical_flow_mode is set to 0, otherwise 1.
        implicit none

        integer :: ve_fl_mo
        real(kind=c_double) :: gw_l

        if (gw_l <= -999.d0) then ! Äquatorialer Erdradius beträgt 6378137 m
            ve_fl_mo = 0
        else
            ve_fl_mo = 1
        end if

    end function ve_fl_mo

    subroutine initialize_groundwater_variables(s, dep, thick, water_table_depth, nlayer)
        implicit none

        type(groundwater_variables), intent(inout) :: s ! groundwater_variables instance
        real(c_double), dimension(number_of_layers), intent(in) :: dep ! m, z-coordinate of soil layers centres
        real(c_double), dimension(number_of_layers), intent(in) :: thick ! mm, Thicks of soil layers
        real(c_double), intent(in) :: water_table_depth ! m, negative below soil surface
        integer(c_int), intent(in) :: nlayer ! Number of soil layers

        s%dep(:nlayer) = dep(:nlayer)
        s%asdf = 0

        call update_groundwater_variables(s, water_table_depth, nlayer, thick)

    end subroutine

    subroutine update_groundwater_variables(s, water_table_depth, nlayer, thick)
        implicit none

        type(groundwater_variables), intent(inout) :: s ! groundwater_variables instance
        real(c_double), intent(in) :: water_table_depth ! m, negative below soil surface
        integer(c_int), intent(in) :: nlayer ! Number of soil layers
        real(c_double), dimension(number_of_layers), intent(in) :: thick ! mm, Thicks of soil layers

        integer :: i
        real(c_double) :: lcl_temp ! m, lowest considered level

        s%vertical_flow_mode = ve_fl_mo(water_table_depth)
        s%water_table_depth = water_table_depth


        ! Find the index of layer in which water table is located
        lcl_temp = 0._c_double
        s%N_groundwater = 0_c_int
        do i = 1, nlayer+1
            if (lcl_temp < water_table_depth) then
                ! Leave the loop
                exit
            else if (i <= nlayer) then
                lcl_temp = s%dep(i) - thick(i)/2000._c_double
                s%N_groundwater = s%N_groundwater + 1_c_int
            else
                ! Water table depth below all soil layers
                s%N_groundwater = s%N_groundwater + 1_c_int
            end if
        end do


    end subroutine


    subroutine adjust_wrt_groundwater(nlayer, gwv, swatmx, rhowg, swati, wetnes, psim)
        ! Adjust soil water related variables with respect to groundwater information
        implicit none

        integer(c_int), intent(in) :: nlayer
        type(groundwater_variables), intent(in) :: gwv
        real(c_double), dimension(nlayer), intent(in) :: swatmx
        real(c_double), intent(in) :: rhowg
        real(c_double), dimension(nlayer), intent(inout) :: swati, wetnes, psim

        ! In case of considering groundwater, overwrite water content, saturation and matric potential
        ! in the layers N_groundwater down to layer NLAYER. PSIM can now be positive.
        if (gwv%vertical_flow_mode == 1 .and. gwv%N_groundwater <= nlayer) then
            swati(gwv%N_groundwater:nlayer) = swatmx(gwv%N_groundwater:nlayer)
            wetnes(gwv%N_groundwater:nlayer) = 1.d0
            psim(gwv%N_groundwater:nlayer) = (gwv%water_table_depth - gwv%dep(gwv%N_groundwater:nlayer))*1000.d0*rhowg
        end if

    end subroutine

end module mod_typedefs
