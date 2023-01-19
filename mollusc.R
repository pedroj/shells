mollusc<-function (n_s = 1000, n_t = 4000, alpha = 80, beta = 40, phi = 55, 
    mu = 30, Omega = 10, s_min = -270, s_max = 62, A = 25, a = 12, 
    b = 16, P = 2, W_1 = 1, W_2 = 1, N = 0, L = 0, D = 1, theta_start = 0, 
    theta_end = 10 * pi) 
{
    alpha <- alpha * pi/180
    beta <- beta * pi/180
    phi <- phi * pi/180
    mu <- mu * pi/180
    Omega <- Omega * pi/180
    s_min <- s_min * pi/180
    s_max <- s_max * pi/180
    P <- P * pi/180
    W_1 <- W_1 * pi/180
    W_2 <- W_2 * pi/180
    data.frame(expand.grid(seq(s_min, s_max, (s_max - s_min)/(n_s - 
        1)), 
		seq(theta_start, theta_end, (theta_end - theta_start)/(n_t - 
        1))) %>% 
		dplyr::rename(s = Var1, theta = Var2)) %>% 
		dplyr::mutate(f_theta = ifelse(N == 
        0, Inf, 360/N * (theta * N/360 - round(theta * N/360, 
        0))), 
		R_e = (a^(-2) * (cos(s))^2 + b^(-2) * (sin(s))^2)^(-0.5), 
        k = L * exp(-(2 * (s - P)/W_1)^2) * exp(-(2 * f_theta/W_2)^2), 
        R = R_e + k, 
		x = D * (A * sin(beta) * cos(theta) + R * 
            cos(s + phi) * cos(theta + Omega) - R * sin(mu) * 
            sin(s + phi) * sin(theta)) * exp(theta/tan(alpha)), 
        y = (-A * sin(beta) * sin(theta) - R * cos(s + phi) * 
            sin(theta + Omega) - R * sin(mu) * sin(s + phi) * 
            cos(theta)) * exp(theta/tan(alpha)), 
			z = (-A * cos(beta) + 
			     R * sin(s + phi) * cos(mu)) * exp(theta/tan(alpha))) %>% 
        dplyr::select(x, y, z)
}
mollusc()

