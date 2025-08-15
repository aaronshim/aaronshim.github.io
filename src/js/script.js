(function () {
  // Theme management
  const THEME_KEY = "site-theme";
  const themes = ["auto", "light", "dark"];
  const themeIcons = {
    auto: "🌓",
    light: "☀️",
    dark: "🌙",
  };

  function getCurrentTheme() {
    return localStorage.getItem(THEME_KEY) || "auto";
  }

  function setTheme(theme) {
    if (!themes.includes(theme)) {
      theme = "auto";
    }

    document.body.className = theme;
    localStorage.setItem(THEME_KEY, theme);

    const toggleButton = document.getElementById("theme-toggle");
    const themeIcon = toggleButton?.querySelector(".theme-icon");
    if (themeIcon) {
      themeIcon.textContent = themeIcons[theme];
    }

    // Update aria-label for accessibility
    if (toggleButton) {
      toggleButton.setAttribute(
        "aria-label",
        `Current theme: ${theme}. Click to cycle themes.`
      );
    }
  }

  function cycleTheme() {
    const currentTheme = getCurrentTheme();
    const currentIndex = themes.indexOf(currentTheme);
    const nextIndex = (currentIndex + 1) % themes.length;
    setTheme(themes[nextIndex]);
  }

  function initTheme() {
    // Set initial theme
    const savedTheme = getCurrentTheme();
    setTheme(savedTheme);

    // Add event listener for theme toggle
    const toggleButton = document.getElementById("theme-toggle");
    if (toggleButton) {
      toggleButton.addEventListener("click", cycleTheme);
    }
  }

  function initTOC() {
    const toc = document.getElementById('toc');
    if (toc) {
      const tocContent = toc.querySelector('ul');
      if (tocContent) {
        const header = document.createElement('div');
        header.className = 'toc-header';

        const title = document.createElement('h3');
        title.className = 'toc-title';
        title.textContent = 'Table of Contents';

        const toggle = document.createElement('button');
        toggle.className = 'toc-toggle';
        toggle.textContent = '[hide]';

        header.appendChild(title);
        header.appendChild(toggle);
        toc.insertBefore(header, tocContent);

        toggle.addEventListener('click', function() {
          toc.classList.toggle('collapsed');
          if (toc.classList.contains('collapsed')) {
            toggle.textContent = '[show]';
          } else {
            toggle.textContent = '[hide]';
          }
        });
      }
    }
  }

  function initActiveTOC() {
    const headings = document.querySelectorAll('h2, h3, h4, h5, h6');
    const tocLinks = document.querySelectorAll('#toc a');

    if (headings.length === 0 || tocLinks.length === 0) {
      return; // No headings or TOC links to process
    }

    function highlightActiveLink() {
      let activeLinkFound = false;
      for (let i = headings.length - 1; i >= 0; i--) {
        const heading = headings[i];
        const rect = heading.getBoundingClientRect();

        // Check if the heading is in the viewport (or slightly above)
        // Adjust the offset as needed to fine-tune when a heading becomes "active"
        const offset = 100; // pixels from top of viewport
        if (rect.top <= offset && rect.bottom > 0) {
          const id = heading.id;
          if (id) {
            tocLinks.forEach(link => {
              link.classList.remove('active');
            });
            const correspondingLink = document.querySelector(`#toc a[href="#${id}"]`);
            if (correspondingLink) {
              correspondingLink.classList.add('active');
              activeLinkFound = true;
            }
          }
          break; // Found the active heading, no need to check further
        }
      }

      // If no heading is active (e.g., at the very top of the page before the first heading)
      if (!activeLinkFound) {
        tocLinks.forEach(link => {
          link.classList.remove('active');
        });
      }
    }

    // Initial highlight on load
    highlightActiveLink();

    // Highlight on scroll
    window.addEventListener('scroll', highlightActiveLink);
    // Also highlight on resize, as element positions might change
    window.addEventListener('resize', highlightActiveLink);
  }

  function init() {
    initTheme();
    initTOC();
    initActiveTOC();
  }

  // Initialize when DOM is ready
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
