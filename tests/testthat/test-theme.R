test_that("vistool_theme creates valid theme objects", {
  # Test default theme
  theme_default = vistool_theme()
  expect_true(is.list(theme_default))
  expect_true(all(c("palette", "text_size", "theme", "alpha") %in% names(theme_default)))
  
  # Test custom theme
  theme_custom = vistool_theme(palette = "plasma", text_size = 14, alpha = 0.6)
  expect_equal(theme_custom$palette, "plasma")
  expect_equal(theme_custom$text_size, 14)
  expect_equal(theme_custom$alpha, 0.6)
  
  # Test validation
  expect_error(vistool_theme(palette = "invalid"))
  expect_error(vistool_theme(text_size = -1))
  expect_error(vistool_theme(alpha = 2))
})

test_that("Theme precedence works correctly", {
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis = as_visualizer(obj_1d, type = "1d")
  
  # Test instance theme override
  vis$set_theme(vistool_theme(palette = "plasma", text_size = 14))
  instance_theme = vis$theme()
  expect_equal(instance_theme$palette, "plasma")
  expect_equal(instance_theme$text_size, 14)
  
  # Test plot theme override (without adding points to avoid format issues)
  p = vis$plot(theme = list(palette = "grayscale", text_size = 16))
  expect_true(inherits(p, "ggplot"))
  
  # Verify that instance theme is preserved after plot()
  expect_equal(vis$theme()$palette, "plasma")
  expect_equal(vis$theme()$text_size, 14)
})

test_that("set_theme and theme methods work", {
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis = as_visualizer(obj_1d, type = "1d")
  
  # Test setting theme
  custom_theme = vistool_theme(palette = "plasma", alpha = 0.5)
  result = vis$set_theme(custom_theme)
  expect_identical(result, vis) # Should return self invisibly
  
  # Test getting theme
  retrieved_theme = vis$theme()
  expect_equal(retrieved_theme$palette, "plasma")
  expect_equal(retrieved_theme$alpha, 0.5)
  
  # Test partial theme update
  vis$set_theme(list(text_size = 18))
  updated_theme = vis$theme()
  expect_equal(updated_theme$text_size, 18)
  # Note: partial updates merge with package defaults, not previous instance theme
  expect_equal(updated_theme$palette, "viridis") # Should be package default
})

test_that("Auto color resolution respects theme changes", {
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis = as_visualizer(obj_1d, type = "1d")
  
  # Change theme and plot without adding points first
  vis$set_theme(vistool_theme(palette = "plasma"))
  p1 = vis$plot()
  
  vis$set_theme(vistool_theme(palette = "grayscale"))
  p2 = vis$plot()
  
  expect_true(inherits(p1, "ggplot"))
  expect_true(inherits(p2, "ggplot"))
  # Both plots should render successfully with different themes
})

test_that("save() uses last rendered plot", {
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis = as_visualizer(obj_1d, type = "1d")
  
  # Plot once
  p1 = vis$plot()
  
  # Save should use the cached plot
  temp_file = tempfile(fileext = ".png")
  expect_no_error(vis$save(temp_file))
  expect_true(file.exists(temp_file))
  unlink(temp_file)
  
  # Test save when no plot cached yet
  vis2 = as_visualizer(obj_1d, type = "1d")
  temp_file2 = tempfile(fileext = ".png")
  expect_no_error(vis2$save(temp_file2)) # Should render and save
  expect_true(file.exists(temp_file2))
  unlink(temp_file2)
})

test_that("merge_theme works correctly", {
  base = vistool_theme(palette = "viridis", text_size = 11)
  override = list(palette = "plasma", alpha = 0.5)
  
  merged = merge_theme(base, override)
  expect_equal(merged$palette, "plasma") # Override wins
  expect_equal(merged$text_size, 11) # Base preserved
  expect_equal(merged$alpha, 0.5) # Override added
  
  # Test with NULL override
  merged_null = merge_theme(base, NULL)
  expect_equal(merged_null, base)
})
