test_that("get_coord_from_string", {
  
  txt = '72503:   New York, La Guardia Airport (United States)\nWIGOS ID: 0-20000-0-72503Latitude: 40-46-00N    Longitude: 073-54-00W    Altitude: 7 m.'
  
  expect_equal(get_coord_from_string(txt), -73.9, tolerance = 1e-6)
  expect_equal(get_coord_from_string(txt, "Latitude"), 40.76667, tolerance = 1e-6)
  
  
  txt = '60101:   Tanger Aerodrome  (Morocco)\nWIGOS ID: 0-20000-0-60101Latitude: 35-43-34N    Longitude: 005-54-19W    Altitude: 14 m.'
  
  expect_equal(get_coord_from_string(txt), -5.905278, tolerance = 1e-6)
  expect_equal(get_coord_from_string(txt, "Latitude"), 35.72611, tolerance = 1e-6)
})