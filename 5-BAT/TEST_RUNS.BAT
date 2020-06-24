rem %1 is D (debug code runs) or C (prod code runs)
rem %2 is subdir Banded or SPARSE
rem %3 id -deb if debug code is to be run or null if production code is to be run

CD \MYSTRAN\%1\Runs\TestRuns\Statics\%2\CurrentRuns

MYSTRAN%3 ALL-ELEM-TEST                         & pause     rem 01
MYSTRAN%3 AQ3U2S004                             & pause     rem 02
MYSTRAN%3 BAR-10-BUCKLING-CF-LOAD-LAN           & pause     rem 03
MYSTRAN%3 BAR-10-BUCKLING-CF-LOAD-LAN-3D        & pause     rem 04
MYSTRAN%3 BAR-AUTOSPC-CHECK                     & pause     rem 05
MYSTRAN%3 BAR-GRAV                              & pause     rem 06
MYSTRAN%3 BAR-I12                               & pause     rem 07
MYSTRAN%3 BAR-OFFSET                            & pause     rem 08
MYSTRAN%3 BAR-PINFLAG                           & pause     rem 09
MYSTRAN%3 BAR-RFORCE-OFFSET                     & pause     rem 10
MYSTRAN%3 BAR-TE-TEST                           & pause     rem 11
MYSTRAN%3 BAR-THERM-CONSTR                      & pause     rem 12
MYSTRAN%3 BAR-THERM-FREE                        & pause     rem 13
MYSTRAN%3 CONM2A                                & pause     rem 14
MYSTRAN%3 CORD3-0                               & pause     rem 15
MYSTRAN%3 EXAMPLE1                              & pause     rem 16
MYSTRAN%3 GUYAN-01-FULL                         & pause     rem 17
MYSTRAN%3 GUYAN-01-SPARSE                       & pause     rem 18
MYSTRAN%3 HEXA08-02-02-020-CANT-AR1-RED-2x2x2   & pause     rem 19
MYSTRAN%3 RADIAL-BARS-CYL-GLOBAL-END-LOADS      & pause     rem 20
MYSTRAN%3 RADIAL-BARS-CYL-GLOBAL-RB-DISP        & pause     rem 21
MYSTRAN%3 RBE2-01-CBAR-01                       & pause     rem 22
MYSTRAN%3 RBE2-01-CROD-03                       & pause     rem 23
MYSTRAN%3 RBE2-01-CROD-05-MPC-03                & pause     rem 24
MYSTRAN%3 RBE2-02-CBAR-03                       & pause     rem 25
MYSTRAN%3 RBE3-01-CBAR-08                       & pause     rem 26
MYSTRAN%3 RODDISPL-N-LOAD                       & pause     rem 27
MYSTRAN%3 RODDISPL-W-LOAD                       & pause     rem 28
MYSTRAN%3 RODLOAD-MPC-FOR-SING                  & pause     rem 29
MYSTRAN%3 SOLVE_EQNS_1_BANDIT                   & pause     rem 30
MYSTRAN%3 SOLVE_EQNS_1_GRID                     & pause     rem 31
MYSTRAN%3 SOLVE_EQNS_1_SEQGP                    & pause     rem 32
MYSTRAN%3 VD0114                                & pause     rem 33

CD \MYSTRAN\%1\Runs\TestRuns\Eigens\%2\CurrentRuns

MYSTRAN%3 ALL-ELEM-TEST-GIV                     & pause     rem 34
MYSTRAN%3 ALL-ELEM-TEST-INV                     & pause     rem 35
MYSTRAN%3 ALL-ELEM-TEST-LANCZOS-MODE-2-DPB      & pause     rem 36
MYSTRAN%3 ALL-ELEM-TEST-MGIV                    & pause     rem 37

MYSTRAN%3 AQ3U2S004-GIV                         & pause     rem 38
MYSTRAN%3 AQ3U2S004-INV                         & pause     rem 39
MYSTRAN%3 AQ3U2S004-LANCZOS-MODE-2-DPB          & pause     rem 40
MYSTRAN%3 AQ3U2S004-MGIV                        & pause     rem 41

MYSTRAN%3 BAR-CC-GIV                            & pause     rem 42
MYSTRAN%3 BAR-CC-INV                            & pause     rem 43
MYSTRAN%3 BAR-CC-MGIV                           & pause     rem 44
MYSTRAN%3 BAR-CC-LANCZOS-MODE-2-DGB             & pause     rem 45
MYSTRAN%3 BAR-CC-LANCZOS-MODE-2-DPB             & pause     rem 46
MYSTRAN%3 BAR-CC-LANCZOS-MODE-3-DGB             & pause     rem 47
MYSTRAN%3 BAR-CC-LANCZOS-MODE-3-DPB             & pause     rem 48

MYSTRAN%3 BAR-FF-GIV                            & pause     rem 49
MYSTRAN%3 BAR-FF-INV                            & pause     rem 50
MYSTRAN%3 BAR-FF-LANCZOS-MODE-2-DGB             & pause     rem 51
MYSTRAN%3 BAR-FF-LANCZOS-MODE-2-DPB             & pause     rem 52
MYSTRAN%3 BAR-FF-LANCZOS-MODE-3-DGB             & pause     rem 53
MYSTRAN%3 BAR-FF-LANCZOS-MODE-3-DPB             & pause     rem 54

MYSTRAN%3 CB-EXAMPLE-12                         & pause     rem 55

MYSTRAN%3 GUYAN-03-GIV                          & pause     rem 56
MYSTRAN%3 GUYAN-03-INV                          & pause     rem 57
MYSTRAN%3 GUYAN-03-LANCZOS                      & pause     rem 58
MYSTRAN%3 GUYAN-03-MGIV                         & pause     rem 59