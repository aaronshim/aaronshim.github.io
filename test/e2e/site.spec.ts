import { assert, assertEquals } from "@std/assert";
import puppeteer from "puppeteer";

const launchBrowser = () => {
  const executablePath = Deno.env.get("PUPPETEER_EXECUTABLE_PATH") || "/usr/bin/chromium";
  return puppeteer.launch({ 
    executablePath,
    args: ["--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage"]
  });
};

Deno.test("Homepage loads correctly", async () => {
  const browser = await launchBrowser();
  const page = await browser.newPage();
  await page.goto("http://localhost");
  
  // Check page title
  const pageTitle = await page.title();
  assertEquals(pageTitle, "Home | My Site Name");
  
  // Check site name appears in header
  const siteName = await page.$eval("h1.header-title a", el => el.textContent);
  assertEquals(siteName, "My Site Name");
  
  await browser.close();
});

Deno.test("Blog posts are listed on homepage", async () => {
  const browser = await launchBrowser();
  const page = await browser.newPage();
  await page.goto("http://localhost");
  
  // Check that blog posts are listed
  const postTitles = await page.$$eval(".line-title a", elements => 
    elements.map(el => el.textContent)
  );
  
  assert(postTitles.includes("Hello, world!"), "Should include 'Hello, world!' post");
  assert(postTitles.includes("¡Hola Mundo!"), "Should include '¡Hola Mundo!' post");
  
  await browser.close();
});

Deno.test("Navigation links work", async () => {
  const browser = await launchBrowser();
  const page = await browser.newPage();
  await page.goto("http://localhost");
  
  // Check navigation menu exists
  const navLinks = await page.$$eval(".header-menu a", elements => 
    elements.map(el => el.textContent)
  );
  
  assert(navLinks.includes("About"), "Should have About link");
  assert(navLinks.includes("Posts"), "Should have Posts link"); 
  assert(navLinks.includes("RSS"), "Should have RSS link");
  
  await browser.close();
});

Deno.test("Theme toggle button exists", async () => {
  const browser = await launchBrowser();
  const page = await browser.newPage();
  await page.goto("http://localhost");
  
  // Check theme toggle button exists
  const themeButton = await page.$("#theme-toggle");
  assert(themeButton !== null, "Theme toggle button should exist");
  
  const themeIcon = await page.$(".theme-icon");
  assert(themeIcon !== null, "Theme icon should exist");
  
  await browser.close();
});

Deno.test("GHC version info appears in footer", async () => {
  const browser = await launchBrowser();
  const page = await browser.newPage();
  await page.goto("http://localhost");
  
  // Check that GHC version appears in footer
  const footerText = await page.$eval("footer", el => el.textContent);
  assert(footerText?.includes("Built with"), "Footer should mention 'Built with'");
  assert(footerText?.includes("GHC"), "Footer should mention GHC");
  
  await browser.close();
});

Deno.test("RSS and sitemap are accessible", async () => {
  const browser = await launchBrowser();
  const page = await browser.newPage();
  
  // Test RSS feed
  const rssResponse = await page.goto("http://localhost/rss.xml");
  assertEquals(rssResponse?.status(), 200, "RSS feed should be accessible");
  
  // Test sitemap  
  const sitemapResponse = await page.goto("http://localhost/sitemap.xml");
  assertEquals(sitemapResponse?.status(), 200, "Sitemap should be accessible");
  
  await browser.close();
});
