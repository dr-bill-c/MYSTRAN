           0

 MYSTRAN Version 6.35     Nov 25 2011 by Dr Bill Case (this ALL FEATURES edition is SP protected)
 *** Please report any problems to the author at drbillc@mystran.com ***                                                                                                                                                                                         

 >> MYSTRAN BEGIN  : 11/26/2011 at 21:17:56.511 The input file is RBE2-01-CBAR-01.DAT                                                                                                                                                                                                                                             

 
 
 OUTPUT FOR SUBCASE        1
 
                                                       D I S P L A C E M E N T S
                                              (in global coordinate system at each grid)
           GRID     COORD      T1            T2            T3            R1            R2            R3
                     SYS
            101        0  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
            102        0  1.000000E-01  1.750000E-02  0.000000E+00  0.000000E+00  0.000000E+00  3.450000E-03
            103        0  1.000000E-01  3.970000E-01  0.000000E+00  0.000000E+00  0.000000E+00  3.450000E-03
                         ------------- ------------- ------------- ------------- ------------- -------------
 MAX (for output set):    1.000000E-01  3.970000E-01  0.000000E+00  0.000000E+00  0.000000E+00  3.450000E-03
 MIN (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00

 ABS (for output set):    1.000000E-01  3.970000E-01  0.000000E+00  0.000000E+00  0.000000E+00  3.450000E-03
 
 
 OUTPUT FOR SUBCASE        1
 
                                                     A P P L I E D    F O R C E S
                                              (in global coordinate system at each grid)
           GRID     COORD      T1            T2            T3            R1            R2            R3
                     SYS
            103        0  1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 
 
 OUTPUT FOR SUBCASE        1
 
                                                          S P C   F O R C E S
                                              (in global coordinate system at each grid)
           GRID     COORD      T1            T2            T3            R1            R2            R3
                     SYS
            101        0 -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00 -3.600000E+02
 
 
 OUTPUT FOR SUBCASE        1
 
                                                          M P C   F O R C E S
                                              (in global coordinate system at each grid)
           GRID     COORD      T1            T2            T3            R1            R2            R3
                     SYS
            102        0  1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.300000E+02
            103        0 -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.637979E-12
                         ------------- ------------- ------------- ------------- ------------- -------------
     MPC FORCE TOTALS:    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.300000E+02
     (for output set)
 
 
 OUTPUT FOR SUBCASE        1
 


                                           G R I D   P O I N T   F O R C E   B A L A N C E
                                             (in global coordinate system at each grid)

                                FORCE BALANCE FOR GRID POINT      101 IN GLOBAL COORD SYSTEM        0

                               T1            T2            T3            R1            R2            R3

 APPLIED FORCE            0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 SPC FORCE               -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00 -3.600000E+02
 MPC FORCE                0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 BAR      ELEM        12  1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.600000E+02
                         ------------- ------------- ------------- ------------- ------------- -------------
 TOTALS                :  0.000000E+00  2.842171E-13  0.000000E+00  0.000000E+00  0.000000E+00 -2.501110E-12
 (should all be 0)


                                FORCE BALANCE FOR GRID POINT      102 IN GLOBAL COORD SYSTEM        0

                               T1            T2            T3            R1            R2            R3

 APPLIED FORCE            0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 SPC FORCE                0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 MPC FORCE                1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.300000E+02
 BAR      ELEM        12 -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00 -3.300000E+02
                         ------------- ------------- ------------- ------------- ------------- -------------
 TOTALS                :  0.000000E+00 -2.842171E-13  0.000000E+00  0.000000E+00  0.000000E+00 -2.273737E-12
 (should all be 0)


                                FORCE BALANCE FOR GRID POINT      103 IN GLOBAL COORD SYSTEM        0

                               T1            T2            T3            R1            R2            R3

 APPLIED FORCE            1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 SPC FORCE                0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00
 MPC FORCE               -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.637979E-12
                         ------------- ------------- ------------- ------------- ------------- -------------
 TOTALS                :  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.637979E-12
 (should all be 0)


                                      Max abs values of force imbalance totals from above grids

                               T1            T2            T3            R1            R2            R3

 Max abs imbal any grid:  0.000000E+00  2.842171E-13  0.000000E+00  0.000000E+00  0.000000E+00  3.637979E-12
 Max abs force any grid:  1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.600000E+02
 as % of max abs force :      0.00E+00%     9.47E-12%                                               1.01E-12%
 Occurs at grid*       :       101           101           101           101           101           103
 (*for output set)
 
 
 OUTPUT FOR SUBCASE        1
 
                                  E L E M   N O D A L   F O R C E S   I N   G L O B A L   C O O R D S
                                               F O R   E L E M E N T   T Y P E   B A R      
      Element     Grid         T1            T2            T3            R1            R2            R3
         ID      Point
             12      101 -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00 -3.600000E+02
                     102  1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.300000E+02
 
                         ------------- ------------- ------------- ------------- ------------- -------------
 MAX (for output set):    1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.300000E+02
 MIN (for output set):   -1.000000E+03 -3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00 -3.600000E+02

 ABS (for output set):    1.000000E+03  3.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  3.600000E+02
 
 
 OUTPUT FOR SUBCASE        1
 
                                                       E L E M E N T   E N G I N E E R I N G   F O R C E S
                                                            F O R   E L E M E N T   T Y P E   B A R      
                  Element       Bend-Moment End A           Bend-Moment End B              - Shear -              Axial         Torque
                     ID       Plane 1       Plane 2       Plane 1       Plane 2      Plane 1       Plane 2        Force
                      12  3.600000E+02  0.000000E+00  3.300000E+02  0.000000E+00  3.000000E+00  0.000000E+00  1.000000E+03  0.000000E+00
                         ------------- ------------- ------------- ------------- ------------- ------------- ------------- -------------
 MAX (for output set):    3.600000E+02  0.000000E+00  3.300000E+02  0.000000E+00  3.000000E+00  0.000000E+00  1.000000E+03  0.000000E+00
 MIN (for output set):    3.600000E+02  0.000000E+00  3.300000E+02  0.000000E+00  3.000000E+00  0.000000E+00  1.000000E+03  0.000000E+00

 ABS (for output set):    3.600000E+02  0.000000E+00  3.300000E+02  0.000000E+00  3.000000E+00  0.000000E+00  1.000000E+03  0.000000E+00
 
 
 OUTPUT FOR SUBCASE        1
 
                             E L E M E N T   S T R E S S E S   I N   L O C A L   E L E M E N T   C O O R D I N A T E   S Y S T E M
                                                          F O R   E L E M E N T   T Y P E   B A R      
                 Element      SA1           SA2           SA3           SA4          Axial         SA-Max        SA-Min      M.S.-T
                    ID        SB1           SB2           SB3           SB4          Stress        SB-Max        SB-Min      M.S.-C
 
                      12  0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05     -1.00E+00
                          0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05     -1.00E+00
                         ------------- ------------- ------------- ------------- ------------- ------------- ------------- ---------
 MAX (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05     -1.00E+00
 MAX (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05     -1.00E+00

 MIN (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05     -1.00E+00
 MIN (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05     -1.00E+00

 ABS (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05      1.00E+00
 ABS (for output set):    0.000000E+00  0.000000E+00  0.000000E+00  0.000000E+00  1.000000E+05  1.000000E+05  1.000000E+05      1.00E+00
