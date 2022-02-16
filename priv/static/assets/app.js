const searchBar = document.getElementById("search-bar")
const searchBarWrapper = document.getElementById("search-bar-wrapper")
const searchSuggestions = document.getElementById("search-suggestions")

searchBar.addEventListener("focus", function() {
  if (searchBar.value.trim() != "") {
    showSuggestions()
  }
})

searchBar.addEventListener("input", function() {
  if (searchBar.value.trim() != "") {
    showSuggestions()
  } else {
    hideSuggestions()
  }
})

searchBar.addEventListener("blur", (e) => {
  if (e.relatedTarget == null || !e.relatedTarget.classList.contains("search-suggestions-entry")) {
    hideSuggestions()
  }
})

function showSuggestions() {
  searchBarWrapper.classList.remove("rounded-b")
  searchSuggestions.classList.remove("hidden")
}

function hideSuggestions() {
  searchBarWrapper.classList.add("rounded-b")
  searchSuggestions.classList.add("hidden")
}
