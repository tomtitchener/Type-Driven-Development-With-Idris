## Exercises from Edwin Brady Type-Driven Development With Idris, Manning Press

    bash-3.2$ idris --testpkg exercises.ipkg 
    Type checking ./Exercises/Exercises_5_3_5.idr
    Type checking ./Exercises/Tests.idr
    Type checking ./Exercises/Exercises_4_3_5.idr
    Type checking ./Exercises/Exercises_4_2_4.idr
    Type checking ./Exercises/Exercises_4_1_5.idr
    Type checking ./Exercises/Exercises_3_3_3.idr
    Type checking ./Exercises/Exercises_3_2_4.idr
    Type checking /var/folders/y5/sp__xwy16_334tfkrphddv4c0000gq/T/idris16807282475249.idr
    Test test_zero_len Passed
    Test test_non_zero_len Passed
    Test test_reverse Passed
    Test test_mapL Passed
    Test test_mapV Passed
    Test test_double_mat Passed
    Test test_mult_mat Passed
    Test test_tree_list Passed
    Test test_expr_eval Passed
    Test test_take_vect Passed
    Test test_sum_entries_ok Passed
    Test test_sum_entries_fail Passed
    Test test_ds_size Passed
    Test test_ds_add_one Passed
    Test test_ds_search Passed


    bash-3.2$ idris --clean exercises.ipkg 
    Removed: Exercises/Exercises_3_2_4.ibc
    Removed: Exercises/Exercises_3_3_3.ibc
    Removed: Exercises/Exercises_4_1_5.ibc
    Removed: Exercises/Exercises_4_2_4.ibc
    Removed: Exercises/Exercises_4_3_5.ibc
    Removed: Exercises/Exercises_5_3_5.ibc


