library("bittermelon")
library("grid")
library("gridpattern")
library("piecepackr")

font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
l_xmpdf <- as_bm_list("xmpdf", font = font)
l_xmpdf[[1]] <- 2L * l_xmpdf[[1]]
l_xmpdf[[2]] <- 2L * l_xmpdf[[2]]
l_xmpdf[[3]] <- 3L * l_xmpdf[[3]]
l_xmpdf[[4]] <- 4L * l_xmpdf[[4]]
l_xmpdf[[5]] <- 4L * l_xmpdf[[5]]
l_xmpdf <- bm_glow(l_xmpdf, corner = TRUE, value = 1L)

xmpdf <- do.call(cbind, l_xmpdf)

# font <- hexfont::unifont()
# emoji <- as_bm_list("🐭", font = font) |> bm_call(cbind)

draw_logo <- function() {
    hex <- pp_shape("convex6")
    grid.newpage()
    grid.draw(hex$shape(gp = gpar(col = NA, fill = "skyblue")))
    vp <- viewport(x=0.52, y=0.60, height=0.3, width=0.8)
    pushViewport(vp)
    plot(xmpdf, col = c(NA_character_, "black", "cyan3", "purple", "red"))
    popViewport()

    # vp <- viewport(x=0.53, y=0.34, height=0.4, width=0.5)
    # grid.oblicubes(coords_emoji, vp=vp, scale = 0.7)

    grid.polygon(x = c(min(hex$npc_coords$x), max(hex$npc_coords$x), 0.5),
                 y = c(0.265, 0.265, 0),
                 gp = gpar(fill = "blue", col = NA))
    grid.polygon(x = c(min(hex$npc_coords$x), max(hex$npc_coords$x),
                       max(hex$npc_coords$x), min(hex$npc_coords$x)),
                 y = c(0.265, 0.265, 0.260, 0.260),
                 gp = gpar(fill = "skyblue", col = NA))
    spacing <- 0.06
    grid.pattern_wave(x = c(min(hex$npc_coords$x), max(hex$npc_coords$x), 0.5),
                 y = c(0.275, 0.275, 0),
                 angle = 0, type = "sine", yoffset = 0.052,
                 spacing = spacing, amplitude = 0.2 * spacing,
                 gp = gpar(fill = "black", col = NA))

    grid.rect(x=0.55, y=0.3, width=0.1, height=0.1, gp=gpar(fill="white", col=NA))
    vp <- viewport(x=0.565, y=0.33, height=0.4, width=0.8)
    pushViewport(vp)
    plot(l_xmpdf[[3]], col = c(NA_character_, "black", "cyan3", "purple", "red"))
    popViewport()

    grid.draw(hex$mat(mat_width = 0.03, gp = gpar(col = NA, fill = "black")))
}

w <- 3.0
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo()
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in",
    res = 72, bg = "transparent")
draw_logo()
dev.off()
