;(function() {
  // Theme management
  const THEME_KEY = 'site-theme';
  const themes = ['auto', 'light', 'dark'];
  const themeIcons = {
    auto: '🌓',
    light: '☀️', 
    dark: '🌙'
  };

  function getCurrentTheme() {
    return localStorage.getItem(THEME_KEY) || 'auto';
  }

  function setTheme(theme) {
    if (!themes.includes(theme)) {
      theme = 'auto';
    }
    
    document.body.className = theme;
    localStorage.setItem(THEME_KEY, theme);
    
    const toggleButton = document.getElementById('theme-toggle');
    const themeIcon = toggleButton?.querySelector('.theme-icon');
    if (themeIcon) {
      themeIcon.textContent = themeIcons[theme];
    }
    
    // Update aria-label for accessibility
    if (toggleButton) {
      toggleButton.setAttribute('aria-label', `Current theme: ${theme}. Click to cycle themes.`);
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
    const toggleButton = document.getElementById('theme-toggle');
    if (toggleButton) {
      // Use both click and touchend for better mobile support
      toggleButton.addEventListener('click', cycleTheme);
      toggleButton.addEventListener('touchend', function(e) {
        e.preventDefault();
        cycleTheme();
      });
      
      // Prevent default behavior on touchstart to avoid double-firing
      toggleButton.addEventListener('touchstart', function(e) {
        e.preventDefault();
      });
    }
  }

  // Initialize theme when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initTheme);
  } else {
    initTheme();
  }
})();
