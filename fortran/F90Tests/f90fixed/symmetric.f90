       program symmetric
c
c  Test a 12th order symmetric multistep method for solving the
c  equation of motion of a single planet circling the Sun (which
c  is fixed in space)
c
c  Written by John K. Prentice, 21 December 1992
c
      implicit none
c
c  declare parameters
c
c     zero             = the number 0.0
c     one_point_five   = the number 1.5
c     gausian_constant = Gausian conatanet in au**(3/2)/day
c                        Note that the universal  
c                        gravitational constant is equal
c                        to the sun times the square of
c                        the Gaussian constant
c
      double precision zero,point_five,one_point_five,
     &                 gaussian_constant
      parameter (zero=0.0,point_five=0.5,one_point_five=1.5,
     &           gaussian_constant=0.01720209895)
c
c  declare variables used in the code
c
      double precision time_step,stop_time,radius,time,
     &                 x_position_exact,y_position_exact,
     &                 x_dot_exact,y_dot_exact,x_error,y_error,
     &                 x_position_numerical(0:12),
     &                 y_position_numerical(0:12),x_dot_numerical,
     &                 y_dot_numerical,constant,gravitational_constant,
     &                 x_acceleration(-1:11),y_acceleration(-1:11),
     &                 exact_kinetic_energy,exact_potential_energy,
     &                 exact_angular_momentum,numerical_total_energy,
     &                 numerical_kinetic_energy,exact_total_energy,
     &                 numerical_potential_energy,
     &                 numerical_angular_momentum,
     &                 exact_velocity_squared,
     &                 numerical_velocity_squared,numerical_radius,
     &                 numerical_radius_squared,kinetic_energy_error,
     &                 potential_energy_error,total_energy_error,
     &                 angular_momentum_error,x_dot_error,y_dot_error,
     &                 x_alpha_sum,y_alpha_sum,x_beta_sum,y_beta_sum,
     &                 beta_factor,alpha(0:12),beta(0:12),gamma(0:12),
     &                 gamma_factor,x_gamma_sum,y_gamma_sum,radius_error
      integer j
c
c   12th order symmetric method coefficients
c
c   Reference:  "Symmetric Multistep Methods for the Numerical
c   Integration of Planetary Orbits", G. D. Quinlan and 
c   S. Tremaine, The Astronomical Journal, 100 (1990), page 1695.
c
c   Note!!  The beta below are actually 53,222,400 times
c   the real beta.  This common factor is divided out in the
c   symmetric multistep calculation itself, in order to minimize
c   round-off
c
      beta_factor = 53222400.0d0
      alpha(0)  =  1.0d0
      beta(0)  = 0.0d0
      alpha(1)  = -2.0d0
      beta(1)  = 90987349.0d0
      alpha(2)  =  2.0d0
      beta(2)  = -229596838.0d0
      alpha(3)  = -1.0d0
      beta(3)  = 812627169.0d0
      alpha(4)  =  0.0d0
      beta(4)  = -1628539944.0d0
      alpha(5)  =  0.0d0
      beta(5)  = 2714971338.0d0
      alpha(6)  =  0.0d0 
      beta(6)  = -3041896548.0d0
      alpha(7)  =  0.0d0
      beta(7)  = 2714971338.0d0
      alpha(8)  =  0.0d0
      beta(8)  = -1628539944.0d0
      alpha(9)  =  -1.0d0
      beta(9)  = 812627169.0d0
      alpha(10) =  2.0d0
      beta(10) = -229596838.0d0
      alpha(11) = -2.0d0
      beta(11) = 90987349.0d0
      alpha(12) =  1.0d0
      beta(12) = 0.0d0
c
c   12th order Cowell predictor coefficients
c
c   Reference:  "Astronomical Papers Prepared for the Use of the
c   American Ephemeris and Nautical Almanac", C. J. Cohen, E. C.
c   Hubbard, and C. Oesterwinter, 22 (1973), page 20-21.
c
c   Note!!  The gamaa below are actually 1,743,565,824,000 times
c   the real gamma.  This common factor is divided out in the
c   Cowell predictor calculation itself, in order to minimize
c   round-off
c
      gamma_factor = 1743565824000.0d0
      gamma(0) = 9072652009253.0d0
      gamma(1) = -39726106418680.0d0
      gamma(2) = 140544566352762.0d0
      gamma(3) = -344579280210129.0d0
      gamma(4) = 613137294629235.0d0
      gamma(5) = -811345852376496.0d0
      gamma(6) = 807012356281740.0d0
      gamma(7) = -602852367932304.0d0
      gamma(8) = 333888089374395.0d0
      gamma(9) = -133228219027160.0d0
      gamma(10) = 36262456774618.0d0
      gamma(11) = -6033724094760.0d0
      gamma(12) = 463483373517.0d0
c
c  initializations
c
      time_step = 0.25d0
      stop_time = 365000.0d0
      radius = 1.0d0
      time = -time_step
c
      print *,
     &    ' Position solution via 12th order symmetric multistep method'
      print *,
     &       ' Velocity solution via 12th order Cowell predictor method'
      print *,'     radius = ', radius,', time step = ',time_step
c
c   define a constant which is needed later by the exact solution
c
      gravitational_constant=gaussian_constant**2
      constant=sqrt(gravitational_constant/radius**3)
c
c   initialize the first 12 numerical values using the exact values
c
      do j=-1,11
          if (j.ge.0) time=time+time_step
          x_position_exact = radius*cos(constant*time)         
          y_position_exact = radius*sin(constant*time)
c
          if (j.ge.0) then
              x_position_numerical(j)=x_position_exact
              y_position_numerical(j)=y_position_exact
          endif
c
          x_acceleration(j) = -gravitational_constant/
     &                                radius**3 * 
     &                                                  x_position_exact
          y_acceleration(j) = -gravitational_constant/
     &                                radius**3 * 
     &                                                  y_position_exact
      enddo
c
c   compute exact kinetic and potential energies, and the
c   angular momentum.  These values are all divided by the mass
c   of the object.  Since they are conserved, they will never change
c   and hence do not have to be recalculated later.
c
      x_dot_exact = -radius * constant * sin(constant*time)
      y_dot_exact =  radius * constant * cos(constant*time) 
      exact_velocity_squared = x_dot_exact**2 + y_dot_exact**2
      exact_kinetic_energy = point_five * exact_velocity_squared
      exact_potential_energy = -gravitational_constant / radius
      exact_total_energy = exact_potential_energy + exact_kinetic_energy
      exact_angular_momentum = x_position_exact * y_dot_exact - 
     &                                    y_position_exact * x_dot_exact
c
c   perform loop over time
c
10    time=time+time_step
c  
c
c   calculate new acceleration of body at time=time-time_step
c
      numerical_radius_squared = x_position_numerical(11)**2 + 
     &                                       y_position_numerical(11)**2
      x_acceleration(11) = -gravitational_constant/
     &                      (numerical_radius_squared)**one_point_five *
     &                                          x_position_numerical(11)
      y_acceleration(11) = -gravitational_constant/
     &                      (numerical_radius_squared)**one_point_five * 
     &                                          y_position_numerical(11)
c
c   numerically solve for the new positions using a 12th order
c   symmetric multistep method.
c
c
c   first sum the first and second terms
c
      x_alpha_sum = zero
      x_beta_sum = zero
      y_alpha_sum = zero
      y_beta_sum = zero
c
      do j=0,11          
          x_alpha_sum = x_alpha_sum + alpha(j) * x_position_numerical(j)
          x_beta_sum = x_beta_sum + beta(j) * x_acceleration(j)
          y_alpha_sum = y_alpha_sum + alpha(j) * y_position_numerical(j)
          y_beta_sum = y_beta_sum + beta(j) * y_acceleration(j)
      enddo
c
      x_position_numerical(12) = -x_alpha_sum + 
     &                             time_step**2*(x_beta_sum/beta_factor)
      y_position_numerical(12) = -y_alpha_sum + 
     &                             time_step**2*(y_beta_sum/beta_factor)
c
c   numerically solve for the new velocities using a 12th order
c   Cowell predictor method.
c
c
c   first sum the gamma terms
c
      x_gamma_sum = zero
      y_gamma_sum = zero
c
      do j=0,12           
          x_gamma_sum = x_gamma_sum + gamma(j) * x_acceleration(11-j)
          y_gamma_sum = y_gamma_sum + gamma(j) * y_acceleration(11-j)
      enddo
c
      x_dot_numerical= (x_position_numerical(11) - 
     &                         x_position_numerical(10))/time_step + 
     &                              time_step*(x_gamma_sum/gamma_factor)
      y_dot_numerical = (y_position_numerical(11) - 
     &                         y_position_numerical(10))/time_step + 
     &                              time_step*(y_gamma_sum/gamma_factor)
c
c   push the stack down one
c
      do j=-1,11
          if (j.ge.0) then
              x_position_numerical(j) = x_position_numerical(j+1)
              y_position_numerical(j) = y_position_numerical(j+1)
          endif
c
          if (j.lt.11) then
              x_acceleration(j) = x_acceleration(j+1)
              y_acceleration(j) = y_acceleration(j+1)
          endif
      enddo
c
      if (time.lt.stop_time) go to 10
c
c   print results 
c
c
c   first compute energies and angular momenta (add divided by the mass
c   of the object
c
      numerical_velocity_squared = x_dot_numerical**2 + 
     &                                                y_dot_numerical**2
      numerical_radius = sqrt(x_position_numerical(12)**2 + 
     &                                      y_position_numerical(12)**2)
      numerical_kinetic_energy = point_five * numerical_velocity_squared
      numerical_potential_energy = -gravitational_constant / 
     &                                                  numerical_radius
      numerical_total_energy = numerical_potential_energy + 
     &                                          numerical_kinetic_energy
      numerical_angular_momentum = 
     &                      x_position_numerical(12) * y_dot_numerical - 
     &                      y_position_numerical(12) * x_dot_numerical
c
c   compute exact values for comparsion to numerical ones
c
      x_position_exact = radius*cos(constant*time)         
      y_position_exact = radius*sin(constant*time)
      x_dot_exact = -radius * constant * sin(constant*time)
      y_dot_exact =  radius * constant * cos(constant*time) 
c
c   next compute relative errors
c         
      radius_error = (radius - numerical_radius)/radius
c
      if (x_position_exact.ne.zero) then
          x_error = (x_position_exact - x_position_numerical(12)) /
     &                                                  x_position_exact
      else
          x_error = x_position_numerical(12)
      endif
c
      if (y_position_exact.ne.zero) then
          y_error = (y_position_exact - y_position_numerical(12)) /
     &                                                  y_position_exact
      else
          y_error = y_position_numerical(12)
      endif
c
      if (x_dot_exact.ne.zero) then
          x_dot_error = (x_dot_exact - x_dot_numerical)/
     &                                                       x_dot_exact
      else
          x_dot_error = x_dot_numerical
      endif
c
      if (y_dot_exact.ne.zero) then
          y_dot_error = (y_dot_exact - y_dot_numerical)/
     &                                                       y_dot_exact
      else
          y_dot_error = y_dot_numerical
      endif
c
      kinetic_energy_error = (exact_kinetic_energy - 
     &                    numerical_kinetic_energy)/exact_kinetic_energy
      potential_energy_error = (exact_potential_energy -
     &                numerical_potential_energy)/exact_potential_energy
      total_energy_error = (exact_total_energy - 
     &                       numerical_total_energy)/exact_total_energy
      angular_momentum_error = (exact_angular_momentum - 
     &                numerical_angular_momentum)/exact_angular_momentum
c
c   then print out errors
c
      print *,' Time = ',time
      print *,'    x rel error  = ',x_error,' y rel error  = ',y_error 
      print *,'    vx rel error = ',x_dot_error,' vy rel error = ',
     &                                                       y_dot_error
      print *,'    KE rel error = ',kinetic_energy_error,
     &                         ' PE rel error = ',potential_energy_error
      print *,'    TE rel error = ',total_energy_error,
     &                         ' AM rel error = ',angular_momentum_error
      print *,'    numerical radius = ',numerical_radius,
     &                               ' radius rel error = ',radius_error
c
      end    
