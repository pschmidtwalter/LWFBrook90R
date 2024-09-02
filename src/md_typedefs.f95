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
    
end module mod_typedefs