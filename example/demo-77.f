      PROGRAM DEMO
      INTEGER N1, N2
      DOUBLE PRECISION X1, X2
      CHARACTER*40 STR
      WRITE (*,*) '========== DEMO PROGRAM BEGIN =========='
      CALL PARAMCARD_GET_I('N1', N1, 10)
      CALL PARAMCARD_GET_I('N2', N2, 20)
      CALL PARAMCARD_GET_D('X1', X1, 3.D0)
      CALL PARAMCARD_GET_D('X2', X2, 4.D0)
      CALL PARAMCARD_GET_S('STR', STR, 'ABC')
      CALL PARAMCARD_SUMMARY
      WRITE (*,*) '========== DEMO PROGRAM END ============'
      STOP
      END
