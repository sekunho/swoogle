const searchBar         = document.getElementById("search-bar")
const searchBarWrapper  = document.getElementById("search-bar-wrapper")
const searchSuggestions = document.getElementById("search-suggestions")
const searchCategory    = document.getElementById("category-options")

searchBar.addEventListener("focus", function() {
  if (searchBar.value.trim() != "") {
    querySuggestions()
  }
})

searchBar.addEventListener("input", function() {
  if (searchBar.value.trim() != "") {
    querySuggestions()
  } else {
    hideSuggestions()
  }
})

searchBar.addEventListener("blur", (e) => {
  if (e.relatedTarget == null || !e.relatedTarget.classList.contains("search-suggestions-entry")) {
    hideSuggestions()
  }
})

function querySuggestions() {
  const query = searchBar.value
  const resource = searchCategory.value

  fetch(`/suggest?query=${query}&resource=${resource}`)
    .then(res => res.text())
    .then(data => {
      searchSuggestions.innerHTML = data

      showSuggestions()
    })
}

function showSuggestions() {
  searchBarWrapper.classList.remove("rounded-b")
  searchSuggestions.classList.remove("hidden")
}

function hideSuggestions() {
  searchBarWrapper.classList.add("rounded-b")
  searchSuggestions.classList.add("hidden")
}
