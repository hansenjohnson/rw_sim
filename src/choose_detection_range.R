## choose_detection_range ##

# short
nrws = 1e4
L = 1
x0 = 5
k = -0.7

ini = init_acoustic(nrws = nrws, L = L, x0 = x0, k = k)
plot_init_acoustic(ini = ini, L = L, x0 = x0, k = k, fig_dir = 'tmp/')

# med
nrws = 1e4
L = 1
x0 = 10
k = -0.6

ini = init_acoustic(nrws = nrws, L = L, x0 = x0, k = k)
plot_init_acoustic(ini = ini, L = L, x0 = x0, k = k, fig_dir = 'tmp/')

# long
nrws = 1e5
L = 1
x0 = 20
k = -0.3

ini = init_acoustic(nrws = nrws, L = L, x0 = x0, k = k)
plot_init_acoustic(ini = ini, L = L, x0 = x0, k = k, fig_dir = 'tmp/')