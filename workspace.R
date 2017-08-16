library(local.extrema)

# demo data
rv <- extrema(slv, 3)
plot_extrema(rv)
print(rv)
class(rv)

# wave data
rv <- extrema((100:1*sin(100:1/5)), 5)
plot_extrema(rv)

# build tar
devtools::build(path = 'C:\\Users\\mflanigan\\Source Code\\GitHub\\Local-Extrema\\build')
devtools::use_build_ignore("build")

# build image
png(filename='slv_i5.png')
rv <- extrema(slv, 25)
plot_extrema(rv)
dev.off()
