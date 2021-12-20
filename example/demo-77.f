      PROGRAM DEMO
      INTEGER A, B
      DOUBLE PRECISION X, Y
      CHARACTER*40 MSG
C
      CALL PARAMCARD_GET_I('A', A, 1)
      CALL PARAMCARD_GET_I('B', B, 2)
      CALL PARAMCARD_GET_D('X', X, 0.3D0)
      CALL PARAMCARD_GET_D('Y', Y, 0.4D0)
      CALL PARAMCARD_GET_S('MSG', MSG, '')
      CALL PARAMCARD_SUMMARY
C
      WRITE (*,*) 'A + B = ', A + B
      WRITE (*,*) 'X + Y = ', X + Y
      IF (MSG .NE. '') WRITE (*,*) MSG
C
      STOP
      END
