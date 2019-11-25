
# generate thumbnail from  image
thumbnail <- function(title, img, href, caption = TRUE) {
  div(class = "col-sm-4",
      a(class = "thumbnail", title = title, href = href,
        img(src = img),
        div(class = if (caption) "caption",
            if (caption) title)
      )
  )
}