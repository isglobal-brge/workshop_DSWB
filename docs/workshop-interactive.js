// Workshop Interactive Features

document.addEventListener('DOMContentLoaded', function() {
    // Fix navbar social icons alignment
    const fixNavbarIcons = () => {
        const socialLinks = document.querySelectorAll('.navbar-nav.ms-auto .nav-item.compact .nav-link');
        socialLinks.forEach(link => {
            // Remove any text nodes (whitespace) between elements
            const childNodes = Array.from(link.childNodes);
            childNodes.forEach(node => {
                if (node.nodeType === 3) { // Text node
                    node.remove();
                }
            });
            
            // Ensure proper structure
            const icon = link.querySelector('i');
            const menuText = link.querySelector('.menu-text');
            if (menuText) {
                menuText.style.display = 'none';
            }
            if (icon) {
                link.style.cssText = 'display: flex !important; align-items: center !important; justify-content: center !important; width: 38px !important; height: 38px !important; padding: 0 !important;';
                icon.style.cssText = 'margin: 0 !important; padding: 0 !important; font-size: 1.2rem !important;';
            }
        });
    };
    
    // Run immediately and after a short delay to catch any Quarto modifications
    fixNavbarIcons();
    setTimeout(fixNavbarIcons, 100);
    setTimeout(fixNavbarIcons, 500);
    // Back to Top Button
    const backToTopButton = document.createElement('button');
    backToTopButton.innerHTML = '<i class="bi bi-arrow-up"></i>';
    backToTopButton.className = 'back-to-top';
    backToTopButton.setAttribute('aria-label', 'Back to top');
    document.body.appendChild(backToTopButton);

    // Show/hide back to top button
    window.addEventListener('scroll', function() {
        if (window.pageYOffset > 300) {
            backToTopButton.classList.add('visible');
        } else {
            backToTopButton.classList.remove('visible');
        }
    });

    // Scroll to top when clicked
    backToTopButton.addEventListener('click', function() {
        window.scrollTo({
            top: 0,
            behavior: 'smooth'
        });
    });

    // Scroll Progress Indicator
    const scrollIndicator = document.createElement('div');
    scrollIndicator.className = 'scroll-indicator';
    document.body.appendChild(scrollIndicator);

    window.addEventListener('scroll', function() {
        const winScroll = document.body.scrollTop || document.documentElement.scrollTop;
        const height = document.documentElement.scrollHeight - document.documentElement.clientHeight;
        const scrolled = (winScroll / height) * 100;
        scrollIndicator.style.width = scrolled + '%';
    });

    // Add fade-in animation to elements as they appear
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -100px 0px'
    };

    const observer = new IntersectionObserver(function(entries) {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('fade-in');
                observer.unobserve(entry.target);
            }
        });
    }, observerOptions);

    // Observe feature cards and resource cards
    document.querySelectorAll('.feature-card, .resource-card').forEach(el => {
        observer.observe(el);
    });

    // Native Quarto code-copy button is used (code-copy: true); no custom copy
    // button here, to avoid a duplicate "Copy" control on each code block.

    // Add smooth scrolling to anchor links
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function(e) {
            e.preventDefault();
            const target = document.querySelector(this.getAttribute('href'));
            if (target) {
                target.scrollIntoView({
                    behavior: 'smooth',
                    block: 'start'
                });
            }
        });
    });

    // Add language labels to code blocks
    document.querySelectorAll('pre.sourceCode').forEach(block => {
        const classes = block.className.split(' ');
        const langClass = classes.find(c => c.startsWith('language-'));
        if (langClass) {
            const language = langClass.replace('language-', '').toUpperCase();
            block.setAttribute('data-language', language);
        }
    });

    // Server status indicator
    document.querySelectorAll('.server-status').forEach(status => {
        // This would typically check actual server status
        // For demo purposes, we'll just show it as online
        status.title = 'Server Online';
    });
});
