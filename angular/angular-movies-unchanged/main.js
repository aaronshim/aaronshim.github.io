"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["main"],{

/***/ 2862:
/*!******************************************************************!*\
  !*** ./projects/movies/src/app/app-shell/app-shell.component.ts ***!
  \******************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AppShellComponent: () => (/* binding */ AppShellComponent)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_15__ = __webpack_require__(/*! rxjs */ 1650);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_16__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_17__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_18__ = __webpack_require__(/*! rxjs */ 3317);
/* harmony import */ var _shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../shared/cdk/track-by */ 5221);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_19__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_router_router_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../shared/router/router.state */ 8202);
/* harmony import */ var _shared_router_get_identifier_of_type_and_layout_util__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../shared/router/get-identifier-of-type-and-layout.util */ 5176);
/* harmony import */ var _data_access_api_resources_genre_resource__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../data-access/api/resources/genre.resource */ 3009);
/* harmony import */ var _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @rx-angular/state/effects */ 7448);
/* harmony import */ var _ui_component_hamburger_button_hamburger_button_component__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../ui/component/hamburger-button/hamburger-button.component */ 1703);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_20__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _ui_component_side_drawer_side_drawer_component__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ../ui/component/side-drawer/side-drawer.component */ 9707);
/* harmony import */ var _ui_component_search_bar_search_bar_component__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ../ui/component/search-bar/search-bar.component */ 2152);
/* harmony import */ var _ui_component_dark_mode_toggle_dark_mode_toggle_component__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ../ui/component/dark-mode-toggle/dark-mode-toggle.component */ 662);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_21__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _shared_cdk_lazy_lazy_directive__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ../shared/cdk/lazy/lazy.directive */ 7988);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_22__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);





















const _c0 = function (a2) {
  return ["/list", "genre", a2];
};
function AppShellComponent_ng_container_0_ui_side_drawer_1_nav_3_a_17_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](0, "a", 22)(1, "div", 15);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](2, "fast-svg", 23);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const genre_r7 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction1"](3, _c0, genre_r7.id));
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵattribute"]("data-uf", "menu-gen-" + genre_r7.id);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtextInterpolate1"](" ", genre_r7.name, " ");
  }
}
const _c1 = function () {
  return ["/list", "category", "popular"];
};
const _c2 = function () {
  return ["/list", "category", "top_rated"];
};
const _c3 = function () {
  return ["/list", "category", "upcoming"];
};
function AppShellComponent_ng_container_0_ui_side_drawer_1_nav_3_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](0, "nav", 12)(1, "h3", 13);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](2, "Discover");
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](3, "a", 14)(4, "div", 15);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](5, "fast-svg", 16);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](6, " Popular ");
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](7, "a", 17)(8, "div", 15);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](9, "fast-svg", 18);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](10, " Top Rated ");
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](11, "a", 19)(12, "div", 15);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](13, "fast-svg", 20);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](14, " Upcoming ");
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](15, "h3", 13);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](16, "Genres");
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](17, AppShellComponent_ng_container_0_ui_side_drawer_1_nav_3_a_17_Template, 4, 5, "a", 21);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const ctx_r5 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](6, _c1));
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](7, _c2));
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](8, _c3));
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](6);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxForOf", ctx_r5.genres$)("rxForTrackBy", ctx_r5.trackByGenre)("rxForStrategy", "idle");
  }
}
const _c4 = function () {
  return [];
};
function AppShellComponent_ng_container_0_ui_side_drawer_1_Template(rf, ctx) {
  if (rf & 1) {
    const _r9 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](0, "ui-side-drawer", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵlistener"]("openedChange", function AppShellComponent_ng_container_0_ui_side_drawer_1_Template_ui_side_drawer_openedChange_0_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵrestoreView"](_r9);
      const ctx_r8 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵresetView"](ctx_r8.ui.sideDrawerOpenToggle(false));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](1, "a", 6);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](2, "fast-svg", 7);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](3, AppShellComponent_ng_container_0_ui_side_drawer_1_nav_3_Template, 18, 9, "nav", 8);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](4, "div", 9)(5, "a", 10);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](6, "fast-svg", 11);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]()()();
  }
  if (rf & 2) {
    const vs_r1 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"]().$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("opened", vs_r1.sideDrawerOpen);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxLet", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](2, _c4));
  }
}
function AppShellComponent_ng_container_0_div_3_ng_container_10_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementContainer"](0, 35);
  }
  if (rf & 2) {
    const ctx_r11 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("lazy", ctx_r11.accountMenuComponent$);
  }
}
function AppShellComponent_ng_container_0_div_3_ng_template_11_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtext"](0, " Loading...");
  }
}
function AppShellComponent_ng_container_0_div_3_Template(rf, ctx) {
  if (rf & 1) {
    const _r16 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](0, "div", 24)(1, "ui-hamburger-button", 25);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵlistener"]("click", function AppShellComponent_ng_container_0_div_3_Template_ui_hamburger_button_click_1_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵrestoreView"](_r16);
      const vs_r1 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"]().$implicit;
      const ctx_r14 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵresetView"](ctx_r14.ui.sideDrawerOpenToggle(!vs_r1.sideDrawerOpen));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](2, "div", 26)(3, "ui-search-bar", 27);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵlistener"]("searchSubmit", function AppShellComponent_ng_container_0_div_3_Template_ui_search_bar_searchSubmit_3_listener($event) {
      _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵrestoreView"](_r16);
      const ctx_r17 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵresetView"](ctx_r17.searchMovie($event));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](4, "ui-dark-mode-toggle");
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](5, "div", 28)(6, "button", 29);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵlistener"]("mouseenter", function AppShellComponent_ng_container_0_div_3_Template_button_mouseenter_6_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵrestoreView"](_r16);
      const ctx_r18 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵresetView"](ctx_r18.ui.loadAccountMenu());
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelement"](7, "div", 30)(8, "fast-svg", 31);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](9, "div", 32);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](10, AppShellComponent_ng_container_0_div_3_ng_container_10_Template, 1, 1, "ng-container", 33);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](11, AppShellComponent_ng_container_0_div_3_ng_template_11_Template, 1, 0, "ng-template", null, 34, _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplateRefExtractor"]);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]()()()();
  }
  if (rf & 2) {
    const _r12 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵreference"](12);
    const ctx_r3 = _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵnextContext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("query", ctx_r3.search$);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](7);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxLet", ctx_r3.accountMenuComponent$)("rxLetSuspense", _r12);
  }
}
function AppShellComponent_ng_container_0_div_4_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](0, "div", 36);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵprojection"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
  }
}
function AppShellComponent_ng_container_0_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](1, AppShellComponent_ng_container_0_ui_side_drawer_1_Template, 7, 3, "ui-side-drawer", 1);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementStart"](2, "div", 2);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](3, AppShellComponent_ng_container_0_div_3_Template, 13, 3, "div", 3);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](4, AppShellComponent_ng_container_0_div_4_Template, 2, 0, "div", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxLet", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](3, _c4));
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxLet", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](4, _c4));
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxLet", _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵpureFunction0"](5, _c4));
  }
}
const _c5 = ["*"];
class AppShellComponent {
  constructor(actionsF) {
    this.actionsF = actionsF;
    this.state = (0,_angular_core__WEBPACK_IMPORTED_MODULE_9__.inject)(_rx_angular_state__WEBPACK_IMPORTED_MODULE_10__.RxState);
    this.router = (0,_angular_core__WEBPACK_IMPORTED_MODULE_9__.inject)(_angular_router__WEBPACK_IMPORTED_MODULE_11__.Router);
    this.document = (0,_angular_core__WEBPACK_IMPORTED_MODULE_9__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_12__.DOCUMENT);
    this.routerState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_9__.inject)(_shared_router_router_state__WEBPACK_IMPORTED_MODULE_1__.RouterState);
    this.effects = (0,_angular_core__WEBPACK_IMPORTED_MODULE_9__.inject)(_rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_13__.RxEffects);
    this.genreResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_9__.inject)(_data_access_api_resources_genre_resource__WEBPACK_IMPORTED_MODULE_3__.GenreResource);
    this.ui = this.actionsF.create();
    this.search$ = this.routerState.select((0,_shared_router_get_identifier_of_type_and_layout_util__WEBPACK_IMPORTED_MODULE_2__.getIdentifierOfTypeAndLayoutUtil)('search', 'list'));
    this.accountMenuComponent$ = this.ui.loadAccountMenu$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_14__.switchMap)(() => Promise.all(/*! import() */[__webpack_require__.e("default-node_modules_rx-angular_template_fesm2022_template-if_mjs"), __webpack_require__.e("common"), __webpack_require__.e("projects_movies_src_app_app-shell_account-menu_account-menu_component_ts")]).then(__webpack_require__.bind(__webpack_require__, /*! ./account-menu/account-menu.component */ 3559)).then(x => x.default)), (0,rxjs__WEBPACK_IMPORTED_MODULE_15__.shareReplay)(1));
    this.genres$ = this.genreResource.getGenresCached();
    this.viewState$ = this.state.select();
    this.trackByGenre = (0,_shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__.trackByProp)('name');
    this.closeSidenav = () => {
      this.ui.sideDrawerOpenToggle(false);
    };
    this.state.set({
      sideDrawerOpen: false
    });
    this.state.connect('sideDrawerOpen', this.ui.sideDrawerOpenToggle$);
    this.effects.register(this.router.events.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_16__.filter)(e => e instanceof _angular_router__WEBPACK_IMPORTED_MODULE_11__.NavigationEnd), (0,rxjs__WEBPACK_IMPORTED_MODULE_17__.map)(e => e.urlAfterRedirects), (0,rxjs__WEBPACK_IMPORTED_MODULE_18__.distinctUntilChanged)()), () => this.closeSidenav());
    /**
     * **🚀 Perf Tip for TBT:**
     *
     * Disable initial sync navigation in router config and schedule it in router-outlet container component.
     * We use a scheduling API (setTimeout) to run it in a separate task from the bootstrap phase
     */
    setTimeout(() => {
      this.router.navigate([
      // The pathname route seems to work correctly on SSR but when pre-rendering it is an empty string.
      // We have to fall back to document URL as a fix.
      (0,_shared_router_router_state__WEBPACK_IMPORTED_MODULE_1__.fallbackRouteToDefault)(this.document.location.pathname || this.document.URL)]);
    });
  }
  searchMovie(term) {
    term === '' ? this.router.navigate(['list/category/popular']) : this.router.navigate([`list/search/${term}`]);
  }
  static #_ = this.ɵfac = function AppShellComponent_Factory(t) {
    return new (t || AppShellComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_19__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵdefineComponent"]({
    type: AppShellComponent,
    selectors: [["app-shell"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵProvidersFeature"]([_rx_angular_state__WEBPACK_IMPORTED_MODULE_10__.RxState, _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_13__.RxEffects, _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_19__.RxActionFactory]), _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵStandaloneFeature"]],
    ngContentSelectors: _c5,
    decls: 1,
    vars: 1,
    consts: [[4, "rxLet"], [3, "opened", "openedChange", 4, "rxLet"], [1, "content-wrapper"], ["class", "ui-toolbar", 4, "rxLet"], ["class", "content", 4, "rxLet"], [3, "opened", "openedChange"], ["aria-label", "Main page", "href", "/", 1, "navigation-header"], ["name", "movies-logo", "size", "150"], ["class", "navigation", 4, "rxLet"], [1, "menu-footer"], ["aria-label", "The movie DB website", "href", "https://www.themoviedb.org/", "target", "_blank", "rel", "noreferrer noopener"], ["name", "tmdb-logo", "width", "75", "height", "30"], [1, "navigation"], [1, "navigation--headline"], ["aria-label", "Popular", "data-uf", "menu-cat-popular", "routerLinkActive", "active", 1, "navigation--link", 3, "routerLink"], [1, "navigation--menu-item"], ["name", "popular", 1, "navigation--menu-item-icon"], ["data-uf", "menu-cat-topRated", "routerLinkActive", "active", 1, "navigation--link", 3, "routerLink"], ["name", "top_rated", 1, "navigation--menu-item-icon"], ["data-uf", "menu-cat-upcoming", "routerLinkActive", "active", 1, "navigation--link", 3, "routerLink"], ["name", "upcoming", 1, "navigation--menu-item-icon"], ["class", "navigation--link", "routerLinkActive", "active", 3, "routerLink", 4, "rxFor", "rxForOf", "rxForTrackBy", "rxForStrategy"], ["routerLinkActive", "active", 1, "navigation--link", 3, "routerLink"], ["name", "genre", 1, "navigation--menu-item-icon"], [1, "ui-toolbar"], ["data-uf", "menu-btn", 1, "ui-toolbar--action", 3, "click"], [1, "ui-toolbar--widget-container"], [3, "query", "searchSubmit"], [1, "account-menu-dropdown"], ["data-uf", "profile-menu-button", "type", "button", "aria-label", "Toggle profile menu", "name", "profile", 1, "profile-button", 3, "mouseenter"], [1, "focus-overlay"], ["name", "account"], ["data-uf", "profile-menu-content", 1, "profile-menu-content"], [3, "lazy", 4, "rxLet", "rxLetSuspense"], ["loading", ""], [3, "lazy"], [1, "content"]],
    template: function AppShellComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵprojectionDef"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵtemplate"](0, AppShellComponent_ng_container_0_Template, 5, 6, "ng-container", 0);
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_9__["ɵɵproperty"]("rxLet", ctx.viewState$);
      }
    },
    dependencies: [_angular_router__WEBPACK_IMPORTED_MODULE_11__.RouterLink, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_20__.RxLet, _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_21__.RxFor, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_22__.FastSvgComponent, _ui_component_hamburger_button_hamburger_button_component__WEBPACK_IMPORTED_MODULE_4__.HamburgerButtonComponent, _ui_component_side_drawer_side_drawer_component__WEBPACK_IMPORTED_MODULE_5__.SideDrawerComponent, _ui_component_search_bar_search_bar_component__WEBPACK_IMPORTED_MODULE_6__.SearchBarComponent, _ui_component_dark_mode_toggle_dark_mode_toggle_component__WEBPACK_IMPORTED_MODULE_7__.DarkModeToggleComponent, _shared_cdk_lazy_lazy_directive__WEBPACK_IMPORTED_MODULE_8__.LazyDirective],
    styles: [".profile-button[_ngcontent-%COMP%] {\n  outline: none;\n  border: none;\n  color: var(--palette-secondary-main);\n  background-color: var(--palette-background-default);\n  position: relative;\n  display: inline-flex;\n  justify-content: center;\n  align-items: center;\n  cursor: pointer;\n}\n.profile-button[_ngcontent-%COMP%]   .focus-overlay[_ngcontent-%COMP%] {\n  position: absolute;\n  border-radius: 100%;\n  width: 200%;\n  height: 200%;\n}\n.profile-button[_ngcontent-%COMP%]:hover   .focus-overlay[_ngcontent-%COMP%] {\n  background-color: rgba(var(--palette-secondary-main-rgb), 0.05);\n}\n\n.account-menu-dropdown[_ngcontent-%COMP%] {\n  position: relative;\n  display: inline-flex;\n  justify-content: center;\n  align-items: center;\n  background-color: var(--palette-background-default);\n}\n.account-menu-dropdown[_ngcontent-%COMP%]   svg[_ngcontent-%COMP%] {\n  z-index: 2;\n  fill: var(--palette-secondary-main);\n}\n.account-menu-dropdown[_ngcontent-%COMP%]:hover   .profile-menu-content[_ngcontent-%COMP%] {\n  visibility: visible;\n  pointer-events: auto;\n}\n.account-menu-dropdown[_ngcontent-%COMP%]   .profile-menu-content[_ngcontent-%COMP%] {\n  visibility: hidden;\n  width: 200px;\n  background: var(--palette-background-default);\n  pointer-events: none;\n  position: absolute;\n  right: 0;\n  top: 30px;\n  min-width: 160px;\n  box-shadow: var(--theme-shadow-dropdown);\n  border: 1px solid var(--palette-divider);\n  background-color: var(--palette-background-paper);\n  z-index: 1300;\n}\n\n.ui-toolbar[_ngcontent-%COMP%] {\n  height: 72px;\n  display: flex;\n  align-items: center;\n  justify-content: space-between;\n  width: 100%;\n  padding: 0 24px;\n}\n@media screen and (min-width: 1298px) {\n  .ui-toolbar--action[_ngcontent-%COMP%] {\n    display: none;\n  }\n}\n@media screen and (max-width: 1299px) {\n  .ui-toolbar[_ngcontent-%COMP%] {\n    position: fixed;\n    top: 0;\n    left: auto;\n    right: 0;\n    z-index: 1100;\n    box-shadow: rgba(0, 0, 0, 0.2) 0 2px 4px -1px, rgba(0, 0, 0, 0.14) 0 4px 5px 0, rgba(0, 0, 0, 0.12) 0 1px 10px 0;\n    background-color: var(--palette-background-paper);\n    height: 64px;\n  }\n}\n@media screen and (max-width: 600px) {\n  .ui-toolbar[_ngcontent-%COMP%] {\n    height: 56px;\n    padding: 0 16px;\n  }\n}\n@media screen and (max-width: 500px) {\n  .ui-toolbar[_ngcontent-%COMP%] {\n    height: 56px;\n    padding: 0 16px;\n    background-color: hsla(0, 0%, 0%, 0);\n  }\n}\n.ui-toolbar--action[_ngcontent-%COMP%] {\n  color: var(--palette-secondary-main);\n}\n.ui-toolbar--widget-container[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  padding: 2rem;\n  margin-left: auto;\n}\n.ui-toolbar--widget-container[_ngcontent-%COMP%]    > [_ngcontent-%COMP%]:not(:first-child) {\n  margin-left: 12px;\n  margin-right: 12px;\n}\n.ui-toolbar--widget-container[_ngcontent-%COMP%]   a[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  color: var(--palette-secondary-main);\n}\n@media screen and (max-width: 1299px) {\n  .ui-toolbar--widget-container[_ngcontent-%COMP%] {\n    padding: 0;\n  }\n  .ui-toolbar--widget-container[_ngcontent-%COMP%]    > [_ngcontent-%COMP%]:not(:first-child) {\n    margin: 0 0 0 8px;\n  }\n}\n\nui-side-drawer[_ngcontent-%COMP%] {\n  grid-area: sidenav;\n}\n\n.menu-footer[_ngcontent-%COMP%] {\n  margin-top: 16px;\n}\n\n.content-wrapper[_ngcontent-%COMP%] {\n  grid-area: content;\n}\n\n@media screen and (max-width: 1299px) {\n  .content-wrapper[_ngcontent-%COMP%] {\n    padding-top: 72px;\n  }\n}\n.content[_ngcontent-%COMP%] {\n  min-height: calc(100vh - 72px);\n  width: 100%;\n  contain: layout;\n}\n\n[_nghost-%COMP%] {\n  min-height: 100vh;\n  background: var(--palette-background-default);\n}\n@media screen and (min-width: 1298px) {\n  [_nghost-%COMP%] {\n    display: grid;\n    grid: \"sidenav  content\" 100vh/25rem 1fr;\n  }\n}\n\n.tmdb-mark[_ngcontent-%COMP%] {\n  display: flex;\n  justify-content: center;\n  margin: 16px 8px;\n}\n.tmdb-mark[_ngcontent-%COMP%]    > img[_ngcontent-%COMP%] {\n  height: 3rem;\n}\n\n@media screen and (min-width: 1298px) {\n  .navigation[_ngcontent-%COMP%], .navigation-header[_ngcontent-%COMP%] {\n    border-right: 1px solid var(--palette-action-disabled-background);\n  }\n}\n\n.navigation-header[_ngcontent-%COMP%] {\n  display: none;\n  width: 100%;\n  height: 18rem;\n  margin-bottom: 2rem;\n  place-items: center;\n}\n.navigation-header[_ngcontent-%COMP%]   .logo-img[_ngcontent-%COMP%] {\n  max-width: 75%;\n}\n@media screen and (min-width: 1298px) {\n  .navigation-header[_ngcontent-%COMP%] {\n    display: grid;\n  }\n}\n\n.navigation[_ngcontent-%COMP%] {\n  display: flex;\n  flex-direction: column;\n  flex: 1;\n  overflow: auto;\n  font-size: var(--text-md);\n  font-weight: bold;\n  contain: content;\n}\n.navigation[_ngcontent-%COMP%]   a[_ngcontent-%COMP%] {\n  outline: none;\n  display: block;\n  margin-bottom: 0.5rem;\n  color: var(--palette-primary-dark);\n  content-visibility: auto;\n  contain-intrinsic-size: 20px;\n}\n.navigation[_ngcontent-%COMP%]   a.active[_ngcontent-%COMP%], .navigation[_ngcontent-%COMP%]   a[_ngcontent-%COMP%]:active {\n  color: var(--palette-secondary-main);\n}\n.navigation[_ngcontent-%COMP%]   a[_ngcontent-%COMP%]:hover {\n  text-decoration: underline;\n}\n.navigation--menu-item[_ngcontent-%COMP%] {\n  padding: 1rem 2rem;\n  line-height: 1;\n  display: flex;\n  align-items: center;\n}\n.navigation--menu-item-icon[_ngcontent-%COMP%] {\n  margin-right: 5px;\n  width: 0.875em;\n  height: auto;\n}\n.navigation--headline[_ngcontent-%COMP%] {\n  color: var(--palette-text-primary);\n  margin: 4rem 0 1rem 1rem;\n  font-size: var(--text-sm);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL2FwcC1zaGVsbC9hcHAtc2hlbGwuY29tcG9uZW50LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL21peGlucy9tZWRpYS5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUdBO0VBQ0UsYUFBQTtFQUNBLFlBQUE7RUFDQSxvQ0FBQTtFQUNBLG1EQUFBO0VBQ0Esa0JBQUE7RUFDQSxvQkFBQTtFQUNBLHVCQUFBO0VBQ0EsbUJBQUE7RUFDQSxlQUFBO0FBRkY7QUFJRTtFQUNFLGtCQUFBO0VBQ0EsbUJBQUE7RUFDQSxXQUFBO0VBQ0EsWUFBQTtBQUZKO0FBTUk7RUFDRSwrREFBQTtBQUpOOztBQVNBO0VBQ0Usa0JBQUE7RUFDQSxvQkFBQTtFQUNBLHVCQUFBO0VBQ0EsbUJBQUE7RUFDQSxtREFBQTtBQU5GO0FBUUU7RUFDRSxVQUFBO0VBQ0EsbUNBQUE7QUFOSjtBQVVJO0VBQ0UsbUJBQUE7RUFDQSxvQkFBQTtBQVJOO0FBWUU7RUFDRSxrQkFBQTtFQUNBLFlBQUE7RUFDQSw2Q0FBQTtFQUNBLG9CQUFBO0VBQ0Esa0JBQUE7RUFDQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLGdCQUFBO0VBQ0Esd0NBQUE7RUFDQSx3Q0FBQTtFQUNBLGlEQUFBO0VBQ0EsYUFBQTtBQVZKOztBQWNBO0VBQ0UsWUFBQTtFQ3hEQSxhQUFBO0VBQ0EsbUJBQUE7RUR5REEsOEJBQUE7RUFDQSxXQUFBO0VBQ0EsZUFBQTtBQVZGO0FFdkNFO0VGb0RFO0lBQ0UsYUFBQTtFQVZKO0FBQ0Y7QUVsREU7RUZrREY7SUFjSSxlQUFBO0lBQ0EsTUFBQTtJQUNBLFVBQUE7SUFDQSxRQUFBO0lBQ0EsYUFBQTtJQUNBLGdIQUFBO0lBRUEsaURBQUE7SUFDQSxZQUFBO0VBWEY7QUFDRjtBRXBFRTtFRndERjtJQTBCSSxZQUFBO0lBQ0EsZUFBQTtFQVZGO0FBQ0Y7QUVoRkU7RUY4REY7SUErQkksWUFBQTtJQUNBLGVBQUE7SUFDQSxvQ0FBQTtFQVRGO0FBQ0Y7QUFXRTtFQUNFLG9DQUFBO0FBVEo7QUFZRTtFQ3JGQSxhQUFBO0VBQ0EsbUJBQUE7RUFDQSx1QkFBQTtFRHFGRSxhQUFBO0VBQ0EsaUJBQUE7QUFSSjtBQVVJO0VBQ0UsaUJBQUE7RUFDQSxrQkFBQTtBQVJOO0FBV0k7RUN6R0YsYUFBQTtFQUNBLG1CQUFBO0VEMEdJLG9DQUFBO0FBUk47QUU5RkU7RUYwRkE7SUFnQkksVUFBQTtFQVJKO0VBU0k7SUFDRSxpQkFBQTtFQVBOO0FBQ0Y7O0FBWUE7RUFDRSxrQkFBQTtBQVRGOztBQVlBO0VBQ0UsZ0JBQUE7QUFURjs7QUFZQTtFQUNFLGtCQUFBO0FBVEY7O0FFbEhFO0VGK0hBO0lBQ0UsaUJBQUE7RUFURjtBQUNGO0FBWUE7RUFDRSw4QkFBQTtFQUNBLFdBQUE7RUFDQSxlQUFBO0FBVkY7O0FBYUE7RUFDRSxpQkFBQTtFQUNBLDZDQUFBO0FBVkY7QUU1SEU7RUZvSUY7SUFJSSxhQUFBO0lBQ0Esd0NBQ0U7RUFUSjtBQUNGOztBQWFBO0VDckpFLGFBQUE7RUFDQSx1QkFBQTtFRHNKQSxnQkFBQTtBQVRGO0FBV0U7RUFDRSxZQUFBO0FBVEo7O0FFM0lFO0VGd0pGOztJQUdJLGlFQUFBO0VBVkY7QUFDRjs7QUFhQTtFQUNFLGFBQUE7RUFDQSxXQUFBO0VBQ0EsYUFBQTtFQUNBLG1CQUFBO0VBQ0EsbUJBQUE7QUFWRjtBQVlFO0VBQ0UsY0FBQTtBQVZKO0FFN0pFO0VGK0pGO0lBWUksYUFBQTtFQVZGO0FBQ0Y7O0FBYUE7RUNqTUUsYUFBQTtFRG1NQSxzQkFBQTtFQUNBLE9BQUE7RUFDQSxjQUFBO0VBQ0EseUJBQUE7RUFDQSxpQkFBQTtFQUNBLGdCQUFBO0FBVkY7QUFZRTtFQUNFLGFBQUE7RUFDQSxjQUFBO0VBQ0EscUJBQUE7RUFDQSxrQ0FBQTtFQUNBLHdCQUFBO0VBQ0EsNEJBQUE7QUFWSjtBQVlJO0VBRUUsb0NBQUE7QUFYTjtBQWNJO0VBQ0UsMEJBQUE7QUFaTjtBQWdCRTtFQUNFLGtCQUFBO0VBQ0EsY0FBQTtFQUNBLGFBQUE7RUFDQSxtQkFBQTtBQWRKO0FBaUJFO0VBQ0UsaUJBQUE7RUFDQSxjQUFBO0VBQ0EsWUFBQTtBQWZKO0FBa0JFO0VBQ0Usa0NBQUE7RUFDQSx3QkFBQTtFQUNBLHlCQUFBO0FBaEJKIiwic291cmNlc0NvbnRlbnQiOlsiQGltcG9ydCAnLi4vdWkvdG9rZW4vbWl4aW5zL21lZGlhJztcbkBpbXBvcnQgJy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcblxuLnByb2ZpbGUtYnV0dG9uIHtcbiAgb3V0bGluZTogbm9uZTtcbiAgYm9yZGVyOiBub25lO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1kZWZhdWx0KTtcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGN1cnNvcjogcG9pbnRlcjtcblxuICAuZm9jdXMtb3ZlcmxheSB7XG4gICAgcG9zaXRpb246IGFic29sdXRlO1xuICAgIGJvcmRlci1yYWRpdXM6IDEwMCU7XG4gICAgd2lkdGg6IDIwMCU7XG4gICAgaGVpZ2h0OiAyMDAlO1xuICB9XG5cbiAgJjpob3ZlciB7XG4gICAgLmZvY3VzLW92ZXJsYXkge1xuICAgICAgYmFja2dyb3VuZC1jb2xvcjogcmdiYSh2YXIoLS1wYWxldHRlLXNlY29uZGFyeS1tYWluLXJnYiksIDAuMDUpO1xuICAgIH1cbiAgfVxufVxuXG4uYWNjb3VudC1tZW51LWRyb3Bkb3duIHtcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1kZWZhdWx0KTtcblxuICBzdmcge1xuICAgIHotaW5kZXg6IDI7XG4gICAgZmlsbDogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gIH1cblxuICAmOmhvdmVyIHtcbiAgICAucHJvZmlsZS1tZW51LWNvbnRlbnQge1xuICAgICAgdmlzaWJpbGl0eTogdmlzaWJsZTtcbiAgICAgIHBvaW50ZXItZXZlbnRzOiBhdXRvO1xuICAgIH1cbiAgfVxuXG4gIC5wcm9maWxlLW1lbnUtY29udGVudCB7XG4gICAgdmlzaWJpbGl0eTogaGlkZGVuO1xuICAgIHdpZHRoOiAyMDBweDtcbiAgICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWJhY2tncm91bmQtZGVmYXVsdCk7XG4gICAgcG9pbnRlci1ldmVudHM6IG5vbmU7XG4gICAgcG9zaXRpb246IGFic29sdXRlO1xuICAgIHJpZ2h0OiAwO1xuICAgIHRvcDogMzBweDtcbiAgICBtaW4td2lkdGg6IDE2MHB4O1xuICAgIGJveC1zaGFkb3c6IHZhcigtLXRoZW1lLXNoYWRvdy1kcm9wZG93bik7XG4gICAgYm9yZGVyOiAxcHggc29saWQgdmFyKC0tcGFsZXR0ZS1kaXZpZGVyKTtcbiAgICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLWJhY2tncm91bmQtcGFwZXIpO1xuICAgIHotaW5kZXg6IDEzMDA7XG4gIH1cbn1cblxuLnVpLXRvb2xiYXIge1xuICBoZWlnaHQ6IDcycHg7XG4gIEBpbmNsdWRlIGQtZmxleC12O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWJldHdlZW47XG4gIHdpZHRoOiAxMDAlO1xuICBwYWRkaW5nOiAwIDI0cHg7XG5cbiAgQGluY2x1ZGUgaXNEZXNrdG9wIHtcbiAgICAmLS1hY3Rpb24ge1xuICAgICAgZGlzcGxheTogbm9uZTtcbiAgICB9XG4gIH1cblxuICBAaW5jbHVkZSBpc01vYmlsZSB7XG4gICAgcG9zaXRpb246IGZpeGVkO1xuICAgIHRvcDogMDtcbiAgICBsZWZ0OiBhdXRvO1xuICAgIHJpZ2h0OiAwO1xuICAgIHotaW5kZXg6IDExMDA7XG4gICAgYm94LXNoYWRvdzogcmdiKDAgMCAwIC8gMjAlKSAwIDJweCA0cHggLTFweCwgcmdiKDAgMCAwIC8gMTQlKSAwIDRweCA1cHggMCxcbiAgICAgIHJnYigwIDAgMCAvIDEyJSkgMCAxcHggMTBweCAwO1xuICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1wYXBlcik7XG4gICAgaGVpZ2h0OiA2NHB4O1xuICB9XG5cbiAgQGluY2x1ZGUgaXNTbWFsbCB7XG4gICAgaGVpZ2h0OiA1NnB4O1xuICAgIHBhZGRpbmc6IDAgMTZweDtcbiAgfVxuXG4gIEBpbmNsdWRlIGlzWFNtYWxsIHtcbiAgICBoZWlnaHQ6IDU2cHg7XG4gICAgcGFkZGluZzogMCAxNnB4O1xuICAgIGJhY2tncm91bmQtY29sb3I6IGhzbCgwZGVnIDAlIDAlIC8gMCUpO1xuICB9XG5cbiAgJi0tYWN0aW9uIHtcbiAgICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gIH1cblxuICAmLS13aWRnZXQtY29udGFpbmVyIHtcbiAgICBAaW5jbHVkZSBkLWZsZXgtdmg7XG4gICAgcGFkZGluZzogMnJlbTtcbiAgICBtYXJnaW4tbGVmdDogYXV0bztcblxuICAgID4gOm5vdCg6Zmlyc3QtY2hpbGQpIHtcbiAgICAgIG1hcmdpbi1sZWZ0OiAxMnB4O1xuICAgICAgbWFyZ2luLXJpZ2h0OiAxMnB4O1xuICAgIH1cblxuICAgIGEge1xuICAgICAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gICAgICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gICAgfVxuXG4gICAgQGluY2x1ZGUgaXNNb2JpbGUge1xuICAgICAgcGFkZGluZzogMDtcbiAgICAgID4gOm5vdCg6Zmlyc3QtY2hpbGQpIHtcbiAgICAgICAgbWFyZ2luOiAwIDAgMCA4cHg7XG4gICAgICB9XG4gICAgfVxuICB9XG59XG5cbnVpLXNpZGUtZHJhd2VyIHtcbiAgZ3JpZC1hcmVhOiBzaWRlbmF2O1xufVxuXG4ubWVudS1mb290ZXIge1xuICBtYXJnaW4tdG9wOiAxNnB4O1xufVxuXG4uY29udGVudC13cmFwcGVyIHtcbiAgZ3JpZC1hcmVhOiBjb250ZW50O1xufVxuXG5AaW5jbHVkZSBpc01vYmlsZSB7XG4gIC5jb250ZW50LXdyYXBwZXIge1xuICAgIHBhZGRpbmctdG9wOiA3MnB4O1xuICB9XG59XG5cbi5jb250ZW50IHtcbiAgbWluLWhlaWdodDogY2FsYygxMDB2aCAtIDcycHgpO1xuICB3aWR0aDogMTAwJTtcbiAgY29udGFpbjogbGF5b3V0O1xufVxuXG46aG9zdCB7XG4gIG1pbi1oZWlnaHQ6IDEwMHZoO1xuICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWJhY2tncm91bmQtZGVmYXVsdCk7XG4gIEBpbmNsdWRlIGlzRGVza3RvcCB7XG4gICAgZGlzcGxheTogZ3JpZDtcbiAgICBncmlkOlxuICAgICAgJ3NpZGVuYXYgIGNvbnRlbnQnIDEwMHZoIC9cbiAgICAgIDI1cmVtIDFmcjtcbiAgfVxufVxuXG4udG1kYi1tYXJrIHtcbiAgQGluY2x1ZGUgZC1mbGV4LWg7XG4gIG1hcmdpbjogMTZweCA4cHg7XG5cbiAgPiBpbWcge1xuICAgIGhlaWdodDogM3JlbTtcbiAgfVxufVxuXG4ubmF2aWdhdGlvbixcbi5uYXZpZ2F0aW9uLWhlYWRlciB7XG4gIEBpbmNsdWRlIGlzRGVza3RvcCB7XG4gICAgYm9yZGVyLXJpZ2h0OiAxcHggc29saWQgdmFyKC0tcGFsZXR0ZS1hY3Rpb24tZGlzYWJsZWQtYmFja2dyb3VuZCk7XG4gIH1cbn1cblxuLm5hdmlnYXRpb24taGVhZGVyIHtcbiAgZGlzcGxheTogbm9uZTtcbiAgd2lkdGg6IDEwMCU7XG4gIGhlaWdodDogMThyZW07XG4gIG1hcmdpbi1ib3R0b206IDJyZW07XG4gIHBsYWNlLWl0ZW1zOiBjZW50ZXI7XG5cbiAgLmxvZ28taW1nIHtcbiAgICBtYXgtd2lkdGg6IDc1JTtcbiAgfVxuXG4gIEBpbmNsdWRlIGlzRGVza3RvcCB7XG4gICAgZGlzcGxheTogZ3JpZDtcbiAgfVxufVxuXG4ubmF2aWdhdGlvbiB7XG4gIEBpbmNsdWRlIGQtZmxleDtcbiAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbiAgZmxleDogMTtcbiAgb3ZlcmZsb3c6IGF1dG87XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1tZCk7XG4gIGZvbnQtd2VpZ2h0OiBib2xkO1xuICBjb250YWluOiBjb250ZW50O1xuXG4gIGEge1xuICAgIG91dGxpbmU6IG5vbmU7XG4gICAgZGlzcGxheTogYmxvY2s7XG4gICAgbWFyZ2luLWJvdHRvbTogMC41cmVtO1xuICAgIGNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktZGFyayk7XG4gICAgY29udGVudC12aXNpYmlsaXR5OiBhdXRvO1xuICAgIGNvbnRhaW4taW50cmluc2ljLXNpemU6IDIwcHg7XG5cbiAgICAmLmFjdGl2ZSxcbiAgICAmOmFjdGl2ZSB7XG4gICAgICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gICAgfVxuXG4gICAgJjpob3ZlciB7XG4gICAgICB0ZXh0LWRlY29yYXRpb246IHVuZGVybGluZTtcbiAgICB9XG4gIH1cblxuICAmLS1tZW51LWl0ZW0ge1xuICAgIHBhZGRpbmc6IDFyZW0gMnJlbTtcbiAgICBsaW5lLWhlaWdodDogMTtcbiAgICBkaXNwbGF5OiBmbGV4O1xuICAgIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIH1cblxuICAmLS1tZW51LWl0ZW0taWNvbiB7XG4gICAgbWFyZ2luLXJpZ2h0OiA1cHg7XG4gICAgd2lkdGg6IDAuODc1ZW07XG4gICAgaGVpZ2h0OiBhdXRvO1xuICB9XG5cbiAgJi0taGVhZGxpbmUge1xuICAgIGNvbG9yOiB2YXIoLS1wYWxldHRlLXRleHQtcHJpbWFyeSk7XG4gICAgbWFyZ2luOiA0cmVtIDAgMXJlbSAxcmVtO1xuICAgIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1zbSk7XG4gIH1cbn1cbiIsIkBtaXhpbiBkLWZsZXgge1xuICBkaXNwbGF5OiBmbGV4O1xufVxuQG1peGluIGQtaW5saW5lLWZsZXgge1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbn1cblxuQG1peGluIGQtZmxleC12IHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC1oIHtcbiAgZGlzcGxheTogZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtdmgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cbiIsIkBtaXhpbiBpc1hTbWFsbCB7XG4gIEBtZWRpYSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDUwMHB4KSB7XG4gICAgQGNvbnRlbnQ7XG4gIH1cbn1cblxuQG1peGluIGlzU21hbGwge1xuICBAbWVkaWEgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA2MDBweCkge1xuICAgIEBjb250ZW50O1xuICB9XG59XG5cbkBtaXhpbiBpc01vYmlsZSB7XG4gIEBtZWRpYSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDEyOTlweCkge1xuICAgIEBjb250ZW50O1xuICB9XG59XG5cbkBtaXhpbiBpc0Rlc2t0b3Age1xuICBAbWVkaWEgc2NyZWVuIGFuZCAobWluLXdpZHRoOiAxMjk4cHgpIHtcbiAgICBAY29udGVudDtcbiAgfVxufVxuXG5AbWl4aW4gaXNMYXJnZURlc2t0b3Age1xuICBAbWVkaWEgc2NyZWVuIGFuZCAobWluLXdpZHRoOiAxMjk4cHgpIHtcbiAgICBAY29udGVudDtcbiAgfVxufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 3287:
/*!****************************************************!*\
  !*** ./projects/movies/src/app/app.base.config.ts ***!
  \****************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   mergeBaseConfig: () => (/* binding */ mergeBaseConfig)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _routes__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./routes */ 9364);
/* harmony import */ var _state_state_app_initializer_provider__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./state/state-app-initializer.provider */ 1044);
/* harmony import */ var _angular_platform_browser__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/platform-browser */ 6480);






const appConfig = {
  providers: [(0,_angular_platform_browser__WEBPACK_IMPORTED_MODULE_2__.provideClientHydration)(), (0,_angular_router__WEBPACK_IMPORTED_MODULE_3__.provideRouter)(_routes__WEBPACK_IMPORTED_MODULE_0__.ROUTES, (0,_angular_router__WEBPACK_IMPORTED_MODULE_3__.withDebugTracing)(),
  /**
   * **🚀 Perf Tip for TBT:**
   *
   * Disable initial sync navigation in router config and schedule it in router-outlet container component
   */
  (0,_angular_router__WEBPACK_IMPORTED_MODULE_3__.withDisabledInitialNavigation)(), (0,_angular_router__WEBPACK_IMPORTED_MODULE_3__.withInMemoryScrolling)({
    /**
     * **💡 UX Tip for InfiniteScroll:**
     *
     * Reset scroll position to top on route change, users could be
     * irritated starting a new list from the bottom of the page.
     *
     * also: otherwise infinite scroll isn't working properly
     */
    scrollPositionRestoration: 'top'
  })),
  // global actions
  _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__.RxActionFactory,
  /**
   * **🚀 Perf Tip for LCP, TTI:**
   *
   * Fetch data visible in viewport on app bootstrap instead of component initialization.
   */
  (0,_state_state_app_initializer_provider__WEBPACK_IMPORTED_MODULE_1__.withGobalStateInitializer)()]
};
function mergeBaseConfig(...configs) {
  return (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.mergeApplicationConfig)(appConfig, ...configs);
}

/***/ }),

/***/ 1166:
/*!**************************************************!*\
  !*** ./projects/movies/src/app/app.component.ts ***!
  \**************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AppComponent: () => (/* binding */ AppComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _shared_zone_less_zone_less_routing_service__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./shared/zone-less/zone-less-routing.service */ 242);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _app_shell_app_shell_component__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./app-shell/app-shell.component */ 2862);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);






function AppComponent_app_shell_0_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "app-shell");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelement"](1, "router-outlet");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
  }
}
const _c0 = function () {
  return [];
};
class AppComponent {
  /**
   *  **🚀 Perf Tip:**
   *
   *  In zone-less applications we have to handle routing manually.
   *  This is a necessity to make it work zone-less but does not make the app faster.
      import { ZonelessRouting } from './shared/zone-agnostic/zone-less-routing.service';
      constructor() {
    inject(ZonelessRouting).init();
  }
   *
   */
  constructor() {
    (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_shared_zone_less_zone_less_routing_service__WEBPACK_IMPORTED_MODULE_0__.ZonelessRouting).init();
  }
  static #_ = this.ɵfac = function AppComponent_Factory(t) {
    return new (t || AppComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineComponent"]({
    type: AppComponent,
    selectors: [["app-root"]],
    decls: 1,
    vars: 2,
    consts: [[4, "rxLet"]],
    template: function AppComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](0, AppComponent_app_shell_0_Template, 2, 0, "app-shell", 0);
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxLet", _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵpureFunction0"](1, _c0));
      }
    },
    dependencies: [_angular_router__WEBPACK_IMPORTED_MODULE_3__.RouterOutlet, _app_shell_app_shell_component__WEBPACK_IMPORTED_MODULE_1__.AppShellComponent, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_4__.RxLet],
    encapsulation: 2,
    changeDetection: 0
  });
}


/***/ }),

/***/ 6271:
/*!***********************************************!*\
  !*** ./projects/movies/src/app/app.config.ts ***!
  \***********************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   appConfig: () => (/* binding */ appConfig)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _app_base_config__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./app.base.config */ 3287);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/common/http */ 4860);
/* harmony import */ var _rx_angular_cdk_render_strategies__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/cdk/render-strategies */ 1927);
/* harmony import */ var _data_access_api_tmdbContentTypeInterceptor__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./data-access/api/tmdbContentTypeInterceptor */ 8402);
/* harmony import */ var _auth_tmdb_http_interceptor_feature__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./auth/tmdb-http-interceptor.feature */ 9419);
/* harmony import */ var _shared_zone_less_custom_zone__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./shared/zone-less/custom-zone */ 4164);
/* harmony import */ var _angular_service_worker__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @angular/service-worker */ 6381);
/* harmony import */ var _environments_environment__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../environments/environment */ 6136);
/* harmony import */ var _data_access_images_image_loader__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ./data-access/images/image-loader */ 1947);











const browserConfig = {
  providers: [(0,_angular_common_http__WEBPACK_IMPORTED_MODULE_6__.provideHttpClient)((0,_angular_common_http__WEBPACK_IMPORTED_MODULE_6__.withInterceptors)([_data_access_api_tmdbContentTypeInterceptor__WEBPACK_IMPORTED_MODULE_1__.tmdbContentTypeInterceptor, _auth_tmdb_http_interceptor_feature__WEBPACK_IMPORTED_MODULE_2__.tmdbReadAccessInterceptor])), (0,_data_access_images_image_loader__WEBPACK_IMPORTED_MODULE_5__.provideTmdbImageLoader)(), (0,_push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_7__.provideFastSVG)({
    url: name => `assets/svg-icons/${name}.svg`
  }),
  /**
   * **🚀 Perf Tip for TBT:**
   *
   * Chunk app bootstrap over APP_INITIALIZER.
   */
  {
    provide: _angular_core__WEBPACK_IMPORTED_MODULE_8__.APP_INITIALIZER,
    useFactory: () => () => new Promise(resolve => {
      setTimeout(() => resolve());
    }),
    deps: [],
    multi: true
  },
  /**
   * **🚀 Perf Tip for TBT, LCP, CLS:**
   *
   * Configure RxAngular to get maximum performance.
   */
  {
    provide: _rx_angular_cdk_render_strategies__WEBPACK_IMPORTED_MODULE_9__.RX_RENDER_STRATEGIES_CONFIG,
    useValue: {
      patchZone: false
    }
  }, /*needed to fix zone-less hydration*/
  {
    provide: _angular_core__WEBPACK_IMPORTED_MODULE_8__.NgZone,
    useClass: _shared_zone_less_custom_zone__WEBPACK_IMPORTED_MODULE_3__.CustomNgZone
  }, (0,_angular_service_worker__WEBPACK_IMPORTED_MODULE_10__.provideServiceWorker)('ngsw-worker.js', {
    enabled: _environments_environment__WEBPACK_IMPORTED_MODULE_4__.environment.production,
    // Register the ServiceWorker as soon as the app is stable
    // or after 30 seconds (whichever comes first).
    registrationStrategy: 'registerWhenStable:30000'
  })]
};
// We provide the config function as closure to be able to inject configuration from the consuming end
const appConfig = () => (0,_app_base_config__WEBPACK_IMPORTED_MODULE_0__.mergeBaseConfig)(browserConfig);

/***/ }),

/***/ 8031:
/*!***********************************************!*\
  !*** ./projects/movies/src/app/app.module.ts ***!
  \***********************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AppModule: () => (/* binding */ AppModule)
/* harmony export */ });
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _angular_platform_browser__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/platform-browser */ 6480);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _app_component__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./app.component */ 1166);
/* harmony import */ var _app_shell_app_shell_component__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./app-shell/app-shell.component */ 2862);
/* harmony import */ var _app_config__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./app.config */ 6271);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);







class AppModule {
  static #_ = this.ɵfac = function AppModule_Factory(t) {
    return new (t || AppModule)();
  };
  static #_2 = this.ɵmod = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineNgModule"]({
    type: AppModule,
    bootstrap: [_app_component__WEBPACK_IMPORTED_MODULE_0__.AppComponent]
  });
  static #_3 = this.ɵinj = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineInjector"]({
    providers: [...(0,_app_config__WEBPACK_IMPORTED_MODULE_2__.appConfig)().providers],
    imports: [_angular_platform_browser__WEBPACK_IMPORTED_MODULE_4__.BrowserModule, _app_shell_app_shell_component__WEBPACK_IMPORTED_MODULE_1__.AppShellComponent]
  });
}

(function () {
  (typeof ngJitMode === "undefined" || ngJitMode) && _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵsetNgModuleScope"](AppModule, {
    declarations: [_app_component__WEBPACK_IMPORTED_MODULE_0__.AppComponent],
    imports: [_angular_platform_browser__WEBPACK_IMPORTED_MODULE_4__.BrowserModule, _angular_router__WEBPACK_IMPORTED_MODULE_5__.RouterOutlet, _app_shell_app_shell_component__WEBPACK_IMPORTED_MODULE_1__.AppShellComponent, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_6__.RxLet]
  });
})();

/***/ }),

/***/ 6641:
/*!*********************************************************************!*\
  !*** ./projects/movies/src/app/auth/access-token-facade.service.ts ***!
  \*********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   AccessTokenFacade: () => (/* binding */ AccessTokenFacade)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _environments_environment__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../environments/environment */ 6136);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ 6575);




class AccessTokenFacade {
  get accessToken() {
    return this._accessToken;
  }
  constructor() {
    this.platformId = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_1__.PLATFORM_ID);
    this._accessToken = _environments_environment__WEBPACK_IMPORTED_MODULE_0__.environment.tmdbApiReadAccessKey;
    if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_2__.isPlatformBrowser)(this.platformId)) {
      // set accessToken if found in localStorage
      const accessToken = window.localStorage.getItem('accessToken');
      accessToken && this.setUserAccessToken(accessToken);
    }
  }
  setUserAccessToken(accessToken) {
    this._accessToken = accessToken;
  }
  resetToReadAccessToken() {
    this._accessToken = _environments_environment__WEBPACK_IMPORTED_MODULE_0__.environment.tmdbApiReadAccessKey;
  }
  static #_ = this.ɵfac = function AccessTokenFacade_Factory(t) {
    return new (t || AccessTokenFacade)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineInjectable"]({
    token: AccessTokenFacade,
    factory: AccessTokenFacade.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 9419:
/*!***********************************************************************!*\
  !*** ./projects/movies/src/app/auth/tmdb-http-interceptor.feature.ts ***!
  \***********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   tmdbReadAccessInterceptor: () => (/* binding */ tmdbReadAccessInterceptor)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _access_token_facade_service__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./access-token-facade.service */ 6641);


const tmdbReadAccessInterceptor = (req, next) => {
  const accessTokenFacade = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_access_token_facade_service__WEBPACK_IMPORTED_MODULE_0__.AccessTokenFacade);
  return next(req.clone({
    setHeaders: {
      Authorization: `Bearer ${accessTokenFacade.accessToken}`
    }
  }));
};

/***/ }),

/***/ 4765:
/*!**********************************************!*\
  !*** ./projects/movies/src/app/constants.ts ***!
  \**********************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   MY_LIST_FALLBACK: () => (/* binding */ MY_LIST_FALLBACK),
/* harmony export */   POSTER_FALLBACK: () => (/* binding */ POSTER_FALLBACK),
/* harmony export */   defaultRedirectRoute: () => (/* binding */ defaultRedirectRoute)
/* harmony export */ });
const POSTER_FALLBACK = `../assets/images/no_poster_available.jpg`;
const MY_LIST_FALLBACK = `../assets/images/nothing.svg`;
const defaultRedirectRoute = 'list/category/popular';

/***/ }),

/***/ 6719:
/*!*******************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/paginate/utils.ts ***!
  \*******************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   getTMDBPaginateOptions: () => (/* binding */ getTMDBPaginateOptions)
/* harmony export */ });
function getTMDBPaginateOptions(options = {}) {
  return {
    page: options.page || 1
  };
}

/***/ }),

/***/ 3009:
/*!*****************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/genre.resource.ts ***!
  \*****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   GenreResource: () => (/* binding */ GenreResource)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _staticRequest__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../staticRequest */ 7981);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/common/http */ 4860);







const URL_GENRE_MOVIE_LIST = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__.baseUrlApiV3, 'genre', 'movie', 'list'].join('/');
class GenreResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_3__.HttpClient);
    this.getGenres = () => this.http.get(URL_GENRE_MOVIE_LIST).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_4__.map)(({
      genres
    }) => genres));
    this.getGenresCached = (0,_staticRequest__WEBPACK_IMPORTED_MODULE_1__.staticRequest)(this.getGenres);
  }
  getGenresDictionaryCached() {
    return this.getGenresCached().pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_4__.map)(i => (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_5__.toDictionary)(i, 'id')));
  }
  static #_ = this.ɵfac = function GenreResource_Factory(t) {
    return new (t || GenreResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineInjectable"]({
    token: GenreResource,
    factory: GenreResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 7782:
/*!******************************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/internal/base-urls.constant.ts ***!
  \******************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   baseUrlApiV3: () => (/* binding */ baseUrlApiV3),
/* harmony export */   baseUrlApiV4: () => (/* binding */ baseUrlApiV4)
/* harmony export */ });
/* harmony import */ var _environments_environment__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../../../environments/environment */ 6136);

const {
  tmdbBaseUrl,
  apiV3,
  apiV4
} = _environments_environment__WEBPACK_IMPORTED_MODULE_0__.environment;
const baseUrlApiV3 = [tmdbBaseUrl, apiV3].join('/');
const baseUrlApiV4 = [tmdbBaseUrl, apiV4].join('/');

/***/ }),

/***/ 1405:
/*!*****************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/movie.resource.ts ***!
  \*****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   MovieResource: () => (/* binding */ MovieResource)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _paginate_utils__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../paginate/utils */ 6719);
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _sort_utils__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../sort/utils */ 3075);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common/http */ 4860);







const base = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__.baseUrlApiV3, 'movie'].join('/');
const URL_MOVIE_CATEGORY = category => [base, category].join('/');
const URL_MOVIE = id => `${[base, id].join('/')}`;
const URL_MOVIE_CREDITS = id => [URL_MOVIE(id), 'credits'].join('/');
const URL_MOVIE_RECOMMENDATIONS = id => [URL_MOVIE(id), 'recommendations'].join('/');
const URL_MOVIE_QUERY = query => `${_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_1__.baseUrlApiV3}/search/movie?query=${query}`;
class MovieResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_4__.HttpClient);
    this.getMoviesRecommendations = (id, params = {}) => {
      params = getTMDBMovieOptions(params);
      return this.http.get(URL_MOVIE_RECOMMENDATIONS(id), {
        params: params
      });
    };
    this.getMovie = (id, params = {
      append_to_response: 'videos'
    }) => this.http.get(URL_MOVIE(id), {
      params
    });
    this.getCredits = id => this.http.get(URL_MOVIE_CREDITS(id));
    this.getMovieCategory = (category, params = {}) => {
      params = getTMDBMovieOptions(params);
      return this.http.get(URL_MOVIE_CATEGORY(category), {
        params: params
      });
    };
    this.queryMovie = query => this.http.get(URL_MOVIE_QUERY(query)).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_5__.map)(res => res.results));
  }
  static #_ = this.ɵfac = function MovieResource_Factory(t) {
    return new (t || MovieResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineInjectable"]({
    token: MovieResource,
    factory: MovieResource.ɵfac,
    providedIn: 'root'
  });
}

function getTMDBMovieOptions(options) {
  const discoverOptions = {
    ...(0,_paginate_utils__WEBPACK_IMPORTED_MODULE_0__.getTMDBPaginateOptions)(options),
    ...(0,_sort_utils__WEBPACK_IMPORTED_MODULE_2__.getTMDBSortOptions)(options)
  };
  return discoverOptions;
}

/***/ }),

/***/ 3075:
/*!***************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/sort/utils.ts ***!
  \***************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   getTMDBSortOptions: () => (/* binding */ getTMDBSortOptions)
/* harmony export */ });
function getTMDBSortOptions(options = {}) {
  return {
    sort_by: options.sort_by || 'popularity.desc'
  };
}

/***/ }),

/***/ 7981:
/*!******************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/staticRequest.ts ***!
  \******************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   staticRequest: () => (/* binding */ staticRequest)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! rxjs */ 5806);

/**
 * used for single requests which needs to be fetched only one time and afterwards it is enough to replay e.g. languages, menu items, dynamic app configs, etc.
 * @param fn
 */
function staticRequest(fn) {
  let _g;
  return () => {
    if (!_g) {
      _g = fn().pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_0__.publishReplay)(1));
      _g.connect();
    }
    return _g;
  };
}

/***/ }),

/***/ 8402:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/tmdbContentTypeInterceptor.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   tmdbContentTypeInterceptor: () => (/* binding */ tmdbContentTypeInterceptor)
/* harmony export */ });
const tmdbContentTypeInterceptor = (req, next) => {
  return next(req.clone({
    setHeaders: {
      'Content-Type': 'application/json;charset=utf-8'
    }
  }));
};

/***/ }),

/***/ 1947:
/*!********************************************************************!*\
  !*** ./projects/movies/src/app/data-access/images/image-loader.ts ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   provideTmdbImageLoader: () => (/* binding */ provideTmdbImageLoader)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _image_sizes__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./image-sizes */ 4082);
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../constants */ 4765);



const provideTmdbImageLoader = (cfg = {
  baseUrl: `https://image.tmdb.org/t/p/w`
}) => {
  const {
    baseUrl
  } = cfg;
  return {
    provide: _angular_common__WEBPACK_IMPORTED_MODULE_2__.IMAGE_LOADER,
    useValue: config => {
      if (!config.width) {
        config.width = _image_sizes__WEBPACK_IMPORTED_MODULE_0__.W154H205.WIDTH;
      }
      if (config.src === _constants__WEBPACK_IMPORTED_MODULE_1__.POSTER_FALLBACK) {
        return _constants__WEBPACK_IMPORTED_MODULE_1__.POSTER_FALLBACK;
      } else if (config.src === _constants__WEBPACK_IMPORTED_MODULE_1__.MY_LIST_FALLBACK) {
        return _constants__WEBPACK_IMPORTED_MODULE_1__.MY_LIST_FALLBACK;
      }
      return `${baseUrl}${config.width}${config.src}`;
    }
  };
};

/***/ }),

/***/ 4082:
/*!*******************************************************************!*\
  !*** ./projects/movies/src/app/data-access/images/image-sizes.ts ***!
  \*******************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   W154H205: () => (/* binding */ W154H205),
/* harmony export */   W185H278: () => (/* binding */ W185H278),
/* harmony export */   W300H450: () => (/* binding */ W300H450),
/* harmony export */   W44H66: () => (/* binding */ W44H66),
/* harmony export */   W500H282: () => (/* binding */ W500H282),
/* harmony export */   W780H1170: () => (/* binding */ W780H1170),
/* harmony export */   W92H138: () => (/* binding */ W92H138)
/* harmony export */ });
const W44H66 = {
  SIZE: '44w',
  WIDTH: 44,
  HEIGHT: 66
};
const W92H138 = {
  SIZE: '92w',
  WIDTH: 92,
  HEIGHT: 138
};
const W154H205 = {
  SIZE: '154w',
  WIDTH: 154,
  HEIGHT: 205
};
const W185H278 = {
  SIZE: '185w',
  WIDTH: 185,
  HEIGHT: 278
};
const W300H450 = {
  SIZE: '300w',
  WIDTH: 300,
  HEIGHT: 450
};
const W500H282 = {
  SIZE: '500w',
  WIDTH: 500,
  HEIGHT: 282
};
const W780H1170 = {
  SIZE: '780w',
  WIDTH: 780,
  HEIGHT: 1170
};

/***/ }),

/***/ 9364:
/*!*******************************************!*\
  !*** ./projects/movies/src/app/routes.ts ***!
  \*******************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ROUTES: () => (/* binding */ ROUTES)
/* harmony export */ });
// import { RouteISRConfig } from 'ngx-isr';
const ROUTES = [
/**
 * **🚀 Perf Tip for TTI, TBT:**
 *
 * If you have routes with the same UI but different data implement it with 2 parameters instead of 2 different routes.
 * This saves creation-time and destruction-time of the component and also render work in the browser.
 *
 * E.g.:
 *
 * _Bad_
 *  {
 *  path: 'list-category/:category',
 *  loadComponent: import('list.component')
 *  },
 *  {
 *  path: 'list-genre/:genre',
 *  loadComponent: import('list.component')
 *  }
 *
 * _Good_
 * {
 *  path: 'list/:type/:identifier',
 *  loadComponent: import('list.component')
 *  }
 *
 */
{
  path: 'list/:type/:identifier',
  loadComponent: () => Promise.all(/*! import() */[__webpack_require__.e("default-node_modules_rx-angular_template_fesm2022_template-if_mjs"), __webpack_require__.e("default-projects_movies_src_app_ui_pattern_movie-list_movie-list_component_ts"), __webpack_require__.e("common"), __webpack_require__.e("projects_movies_src_app_pages_movie-list-page_movie-list-page_component_ts")]).then(__webpack_require__.bind(__webpack_require__, /*! ./pages/movie-list-page/movie-list-page.component */ 9074))
  // data: { revalidate: 10 } as RouteISRConfig
}, {
  path: 'detail/movie/:identifier',
  loadComponent: () => Promise.all(/*! import() */[__webpack_require__.e("default-node_modules_rx-angular_template_fesm2022_template-if_mjs"), __webpack_require__.e("default-projects_movies_src_app_ui_pattern_movie-list_movie-list_component_ts"), __webpack_require__.e("common"), __webpack_require__.e("projects_movies_src_app_pages_movie-detail-page_movie-detail-page_component_ts")]).then(__webpack_require__.bind(__webpack_require__, /*! ./pages/movie-detail-page/movie-detail-page.component */ 4909))
}, {
  path: 'detail/list/:identifier',
  loadChildren: () => __webpack_require__.e(/*! import() */ "projects_movies_src_app_pages_account-feature_list-detail-page_list-detail-page_routes_ts").then(__webpack_require__.bind(__webpack_require__, /*! ./pages/account-feature/list-detail-page/list-detail-page.routes */ 2287))
}, {
  path: 'detail/person/:identifier',
  loadComponent: () => Promise.all(/*! import() */[__webpack_require__.e("default-node_modules_rx-angular_template_fesm2022_template-if_mjs"), __webpack_require__.e("default-projects_movies_src_app_ui_pattern_movie-list_movie-list_component_ts"), __webpack_require__.e("common"), __webpack_require__.e("projects_movies_src_app_pages_person-detail-page_person-detail-page_component_ts")]).then(__webpack_require__.bind(__webpack_require__, /*! ./pages/person-detail-page/person-detail-page.component */ 2140))
}, {
  path: 'account',
  loadChildren: () => __webpack_require__.e(/*! import() */ "projects_movies_src_app_pages_account-feature_account-feature-page_routes_ts").then(__webpack_require__.bind(__webpack_require__, /*! ./pages/account-feature/account-feature-page.routes */ 8986))
}, {
  path: 'page-not-found',
  loadComponent: () => __webpack_require__.e(/*! import() */ "projects_movies_src_app_pages_not-found-page_not-found-page_component_ts").then(__webpack_require__.bind(__webpack_require__, /*! ./pages/not-found-page/not-found-page.component */ 8884))
}, {
  path: '**',
  redirectTo: 'page-not-found'
}];

/***/ }),

/***/ 4369:
/*!****************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/coerceObservable.ts ***!
  \****************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   coerceObservable: () => (/* binding */ coerceObservable)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! rxjs */ 2568);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! rxjs */ 4980);

function coerceObservable(o) {
  return (0,rxjs__WEBPACK_IMPORTED_MODULE_0__.isObservable)(o) ? o : (0,rxjs__WEBPACK_IMPORTED_MODULE_1__.of)(o);
}

/***/ }),

/***/ 2303:
/*!***************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/get.ts ***!
  \***************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   pluck: () => (/* binding */ pluck)
/* harmony export */ });
/**
 * Safely plucks a property value from an object
 * `obj && prop in obj ? obj[prop] : fallback`
 */
function pluck(o, p, f) {
  return o && p in o ? o[p] : f;
}

/***/ }),

/***/ 7988:
/*!*******************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/lazy/lazy.directive.ts ***!
  \*******************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   LazyDirective: () => (/* binding */ LazyDirective)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _cdk_coerceObservable__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../cdk/coerceObservable */ 4369);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 3317);





/**
 * @example
 * Component: (any-component.ts)
 *
 * export const imports = [RouterOutlet, CommonModule, ...];
 * @Component({
 * ...
 * })
 * export class AnyComponent {}
 *
 * File to import from: (any-component.lazy.ts)
 *
 * import { NgModule } from '@angular/core';
 * import { AccountMenuComponent, imports } from './account-menu.component';
 * export const component = AccountMenuComponent;
 * @NgModule({ declarations: [component], imports })
 * export class _ {}
 *
 * Loader Component: other-component.ts
 * import('./any-component.lazy.ts').then(c => c.component)
 */
class LazyDirective extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__.RxState {
  set lazy(component) {
    this.connect('component', (0,_cdk_coerceObservable__WEBPACK_IMPORTED_MODULE_0__.coerceObservable)(component));
  }
  constructor() {
    super();
    this.vCR = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_2__.ViewContainerRef);
    this.hold(
    // avoid recreation of a component with the same class (distinctUntilChanged)
    this.select('component').pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.distinctUntilChanged)()), c => {
      this.vCR.clear();
      this.vCR.createComponent(c);
    });
  }
  static #_ = this.ɵfac = function LazyDirective_Factory(t) {
    return new (t || LazyDirective)();
  };
  static #_2 = this.ɵdir = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineDirective"]({
    type: LazyDirective,
    selectors: [["", "lazy", ""]],
    inputs: {
      lazy: "lazy"
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵInheritDefinitionFeature"]]
  });
}


/***/ }),

/***/ 9135:
/*!****************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/loading/withLoadingEmissions.ts ***!
  \****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   withLoadingEmission: () => (/* binding */ withLoadingEmission)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! rxjs */ 5043);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! rxjs */ 909);

const defaultLoadingProp = 'loading';
function withLoadingEmission(property) {
  const _property = property === undefined ? defaultLoadingProp : property;
  const start = {
    [_property]: true
  };
  const end = {
    [_property]: false
  };
  return o$ => o$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_0__.startWith)(start), (0,rxjs__WEBPACK_IMPORTED_MODULE_1__.endWith)(end));
}

/***/ }),

/***/ 3467:
/*!***************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/optimized-fetch.ts ***!
  \***************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   optimizedFetch: () => (/* binding */ optimizedFetch)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! rxjs */ 4899);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ 1355);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 7047);

/**
 * **🚀 Perf Tip for TTI, TBT:**
 *
 * Avoid over fetching for HTTP get requests to URLs that will not change result quickly.
 *
 * E.g.:
 * Subsequent HTTP get requests to the URLs api/1 -> api/2 -> api/1 can lead to over fetching of api/1 if the first request is still pending
 * The following logic avoids this.
 */
function optimizedFetch(groupSelector, fetch) {
  return o$ => o$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_0__.groupBy)(groupSelector),
  // exhaust by keySelector e.g. url
  (0,rxjs__WEBPACK_IMPORTED_MODULE_1__.map)(t$ => t$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_2__.exhaustMap)(fetch))), (0,rxjs__WEBPACK_IMPORTED_MODULE_3__.mergeAll)());
}

/***/ }),

/***/ 5221:
/*!********************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/track-by.ts ***!
  \********************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   trackByIndex: () => (/* binding */ trackByIndex),
/* harmony export */   trackByProp: () => (/* binding */ trackByProp)
/* harmony export */ });
const trackByProp = prop => (_, item) => item[prop];
const trackByIndex = () => index => index;

/***/ }),

/***/ 5176:
/*!*****************************************************************************************!*\
  !*** ./projects/movies/src/app/shared/router/get-identifier-of-type-and-layout.util.ts ***!
  \*****************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   getIdentifierOfTypeAndLayoutUtil: () => (/* binding */ getIdentifierOfTypeAndLayoutUtil)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! rxjs */ 2476);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ 9736);

const getIdentifierOfTypeAndLayoutUtil = (filterType, filterLayout = 'list') => {
  return (0,rxjs__WEBPACK_IMPORTED_MODULE_0__.pipe)((0,rxjs__WEBPACK_IMPORTED_MODULE_1__.filter)(({
    type,
    layout
  }) => type === filterType && layout === filterLayout), (0,rxjs__WEBPACK_IMPORTED_MODULE_2__.map)(({
    identifier
  }) => identifier));
};

/***/ }),

/***/ 8202:
/*!***************************************************************!*\
  !*** ./projects/movies/src/app/shared/router/router.state.ts ***!
  \***************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   RouterState: () => (/* binding */ RouterState),
/* harmony export */   fallbackRouteToDefault: () => (/* binding */ fallbackRouteToDefault)
/* harmony export */ });
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! rxjs */ 5043);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _cdk_coerceObservable__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../cdk/coerceObservable */ 4369);
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../constants */ 4765);









const fallbackRouteToDefault = route => route !== '/' ? route : _constants__WEBPACK_IMPORTED_MODULE_1__.defaultRedirectRoute;
/**
 * This service maintains the router state and repopulates it to its subscriber.
 */
class RouterState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_2__.RxState {
  setOptions(options) {
    this.hold((0,_cdk_coerceObservable__WEBPACK_IMPORTED_MODULE_0__.coerceObservable)(options), queryParams => this.router.navigate([], {
      queryParams
    }));
  }
  constructor() {
    super();
    this.document = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_4__.DOCUMENT);
    this.router = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_angular_router__WEBPACK_IMPORTED_MODULE_5__.Router);
    this._routerParams$ = this.router.events.pipe((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_6__.select)((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.filter)(event => event instanceof _angular_router__WEBPACK_IMPORTED_MODULE_5__.NavigationEnd), (0,rxjs__WEBPACK_IMPORTED_MODULE_8__.startWith)('anyValue'), (0,rxjs__WEBPACK_IMPORTED_MODULE_9__.map)(() => {
      // This is a naive way to reduce scripting of router service :)
      // Obviously the params relay on routing structure heavily and could be done more dynamically
      const [layout, type, identifier] = fallbackRouteToDefault(new URL(this.document.location.href, /* On SSR pre-render the location data are relative paths instead of valid absolute URLs, that's why we need to construct a new URL, with explicit origin (substituted by mock if pre-rendering) and then only consume pathname as our routing location */
      this.document.location.origin || 'http://mock.domain').pathname).split('/').slice(-3);
      let sortBy = null;
      const [, queryParams] = fallbackRouteToDefault(this.document.location.search).split('?');
      if (queryParams) {
        const [, sortByAndRest] = queryParams?.split('sort_by=') || queryParams;
        sortBy = sortByAndRest?.split('&')?.shift() || null;
      }
      return {
        layout,
        type,
        identifier,
        sortBy
      };
    }),
    // emits if all values are given and set. (filters out undefined values and will not emit if one is undefined)
    (0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_6__.selectSlice)(['layout', 'identifier', 'type', 'sortBy'])));
    this.routerParams$ = this.select();
    this.connect(this._routerParams$);
  }
  static #_ = this.ɵfac = function RouterState_Factory(t) {
    return new (t || RouterState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineInjectable"]({
    token: RouterState,
    factory: RouterState.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 4164:
/*!*****************************************************************!*\
  !*** ./projects/movies/src/app/shared/zone-less/custom-zone.ts ***!
  \*****************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   CustomNgZone: () => (/* binding */ CustomNgZone)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! rxjs */ 8071);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ 9378);


/**
 * Provides a noop like implementation of `NgZone` which does nothing and provides a way to customize behavior.
 * This zone requires explicit calls to framework to perform rendering.
 */
class CustomNgZone {
  get isStable() {
    return this.onStable.getValue();
  }
  constructor() {
    this.hasPendingMicrotasks = true;
    this.hasPendingMacrotasks = true;
    this.onUnstable = new _angular_core__WEBPACK_IMPORTED_MODULE_0__.EventEmitter();
    this.onMicrotaskEmpty = new _angular_core__WEBPACK_IMPORTED_MODULE_0__.EventEmitter();
    this.onStable = new rxjs__WEBPACK_IMPORTED_MODULE_1__.BehaviorSubject(false);
    this.onError = new _angular_core__WEBPACK_IMPORTED_MODULE_0__.EventEmitter();
    /**
     * Notice:
     * This is a hack to delay the emission of isStable for a micro task
     * This helps HttpTransferCache to get its values first from the cache
     */
    (0,rxjs__WEBPACK_IMPORTED_MODULE_2__.timer)(2000).subscribe(() => {
      this.hasPendingMicrotasks = false;
      this.hasPendingMacrotasks = false;
      this.onStable.next(true);
    });
  }
  run(fn, applyThis, applyArgs) {
    return fn.apply(applyThis, applyArgs);
  }
  runGuarded(fn, applyThis, applyArgs) {
    return fn.apply(applyThis, applyArgs);
  }
  runOutsideAngular(fn) {
    return fn();
  }
  runTask(fn, applyThis, applyArgs) {
    return fn.apply(applyThis, applyArgs);
  }
}

/***/ }),

/***/ 3031:
/*!*********************************************************************!*\
  !*** ./projects/movies/src/app/shared/zone-less/is-zone-present.ts ***!
  \*********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   isZonePresent: () => (/* binding */ isZonePresent)
/* harmony export */ });
/**
 * @description
 * returns true if zone.js polyfills are imported, false otherwise
 */
function isZonePresent() {
  return !!window.Zone;
}

/***/ }),

/***/ 242:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/shared/zone-less/zone-less-routing.service.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ZonelessRouting: () => (/* binding */ ZonelessRouting)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _is_zone_present__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./is-zone-present */ 3031);
/* harmony import */ var _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state/effects */ 7448);






/**
 * A small service encapsulating the hacks needed for routing (and bootstrapping) in zone-less applications
 */
class ZonelessRouting extends _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_1__.RxEffects {
  constructor(errorHandler) {
    super(errorHandler);
    this.router = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_router__WEBPACK_IMPORTED_MODULE_3__.Router);
    this.platformId = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_2__.PLATFORM_ID);
    this.ngZone = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_2__.NgZone);
  }
  init() {
    /**
     * **🚀 Perf Tip:**
     *
     * In zone-less applications we have to trigger CD on every `NavigationEnd` event that changes the view.
     * This is a necessity to make it work zone-less, but does not make the app faster.
     */
    if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_4__.isPlatformBrowser)(this.platformId) && !(0,_is_zone_present__WEBPACK_IMPORTED_MODULE_0__.isZonePresent)()) {
      this.register(
      // Filter relevant navigation events for change detection
      this.router.events,
      // In a service we have to use `ApplicationRef#tick` to trigger change detection.
      // In a component we use `ChangeDetectorRef#detectChanges()` as it is less work compared to `ApplicationRef#tick` as it's less work.
      e => {
        if (e instanceof _angular_router__WEBPACK_IMPORTED_MODULE_3__.NavigationEnd) {
          // Inside appRef [NgZone.onMicrotaskEmpty is used to call appRef.tick()](https://github.com/angular/angular/blob/2fc5b70fcedb8ac35b825b245c0ae394dc125244/packages/core/src/application_ref.ts#L769)
          // As the router events are not tracked in a zone-less environment we programmatically schedule onMicrotaskEmpty here to trigger CD after routing occurred
          this.ngZone.onMicrotaskEmpty.next(true);
        }
      });
    }
  }
  static #_ = this.ɵfac = function ZonelessRouting_Factory(t) {
    return new (t || ZonelessRouting)(_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵinject"](_angular_core__WEBPACK_IMPORTED_MODULE_2__.ErrorHandler));
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineInjectable"]({
    token: ZonelessRouting,
    factory: ZonelessRouting.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 6345:
/*!******************************************************!*\
  !*** ./projects/movies/src/app/state/movie.state.ts ***!
  \******************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   MovieState: () => (/* binding */ MovieState)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../shared/cdk/optimized-fetch */ 3467);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../shared/cdk/loading/withLoadingEmissions */ 9135);
/* harmony import */ var _data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../data-access/api/resources/movie.resource */ 1405);
/* harmony import */ var _shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../shared/cdk/get */ 2303);










class MovieState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_4__.RxState {
  constructor() {
    super();
    this.movieResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_2__.MovieResource);
    this.actionsF = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_6__.RxActionFactory();
    this.actions = this.actionsF.create();
    this.fetchMovie = this.actions.fetchMovie;
    this.fetchCategoryMovies = this.actions.fetchCategoryMovies;
    this.categoryMoviesByIdCtx = id => this.select((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.filter)(({
      categoryMovies
    }) => !!categoryMovies), (0,rxjs__WEBPACK_IMPORTED_MODULE_8__.map)(({
      categoryMovies: {
        value,
        loading
      }
    }) => ({
      loading,
      value: (0,_shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__.pluck)(value, id)
    })));
    this.movieByIdCtx = id => this.select((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.map)(({
      movies: {
        value,
        loading
      }
    }) => ({
      loading,
      value: (0,_shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__.pluck)(value, id)
    })));
    (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_5__.DestroyRef).onDestroy(() => this.actionsF.destroy());
    this.connect('movies', this.actions.fetchMovie$.pipe((0,_shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__.optimizedFetch)(id => id, id => {
      return this.movieResource.getMovie(id).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.map)(result => ({
        value: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.toDictionary)([result], 'id')
      })), (0,_shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__.withLoadingEmission)());
    })), (oldState, newPartial) => {
      const resultState = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(oldState?.movies || {}, newPartial);
      resultState.value = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(oldState?.movies?.value || {}, resultState?.value || {});
      return resultState;
    });
    this.connect('categoryMovies', this.actions.fetchCategoryMovies$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.map)(category => ({
      category
    })), (0,_shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__.optimizedFetch)(({
      category
    }) => category, ({
      category
    }) => this.movieResource.getMovieCategory(category).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.map)(paginatedResult => ({
      value: {
        [category]: paginatedResult
      }
    })), (0,_shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__.withLoadingEmission)()))), (oldState, newPartial) => {
      const resultState = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(oldState?.categoryMovies, newPartial);
      resultState.value = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_9__.patch)(oldState?.categoryMovies?.value, resultState?.value);
      return resultState;
    });
  }
  // prefetch categories / movie
  initialize(options) {
    const opts = options;
    if ('category' in opts && opts.category) {
      this.fetchCategoryMovies(opts.category);
    }
    if ('movieId' in opts && opts.movieId) {
      this.fetchMovie(opts.movieId);
    }
  }
  static #_ = this.ɵfac = function MovieState_Factory(t) {
    return new (t || MovieState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵdefineInjectable"]({
    token: MovieState,
    factory: MovieState.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 1044:
/*!*************************************************************************!*\
  !*** ./projects/movies/src/app/state/state-app-initializer.provider.ts ***!
  \*************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   withGobalStateInitializer: () => (/* binding */ withGobalStateInitializer)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _data_access_api_resources_genre_resource__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../data-access/api/resources/genre.resource */ 3009);
/* harmony import */ var _movie_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./movie.state */ 6345);
/* harmony import */ var _shared_router_router_state__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../shared/router/router.state */ 8202);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! rxjs */ 1527);





/**
 * **🚀 Perf Tip for LCP, TTI:**
 *
 * Use `APP_INITIALIZER` and an init method in data services to run data fetching
 * on app bootstrap instead of component initialization.
 */
function withGobalStateInitializer() {
  return [{
    provide: _angular_core__WEBPACK_IMPORTED_MODULE_3__.APP_INITIALIZER,
    useFactory: (movieState, routerState, genreResource) => {
      return () => {
        // sideBar prefetch
        genreResource.getGenresCached().pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_4__.take)(1)).subscribe();
        // initial route prefetch
        routerState.routerParams$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_4__.take)(1)).subscribe(({
          layout,
          type,
          identifier
        }) => {
          // default route
          layout === 'list' && type === 'category' && movieState.initialize({
            category: identifier
          });
          // movie detail route
          layout === 'detail' && type === 'movie' && movieState.initialize({
            movieId: identifier
          });
        });
      };
    },
    deps: [_movie_state__WEBPACK_IMPORTED_MODULE_1__.MovieState, _shared_router_router_state__WEBPACK_IMPORTED_MODULE_2__.RouterState, _data_access_api_resources_genre_resource__WEBPACK_IMPORTED_MODULE_0__.GenreResource],
    multi: true
  }];
}

/***/ }),

/***/ 4788:
/*!*****************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/backdrop/backdrop.component.ts ***!
  \*****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   BackdropComponent: () => (/* binding */ BackdropComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);

class BackdropComponent {
  constructor() {
    this.opened = false;
  }
  static #_ = this.ɵfac = function BackdropComponent_Factory(t) {
    return new (t || BackdropComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: BackdropComponent,
    selectors: [["ui-backdrop"]],
    hostVars: 2,
    hostBindings: function BackdropComponent_HostBindings(rf, ctx) {
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵclassProp"]("opened", ctx.opened);
      }
    },
    inputs: {
      opened: "opened"
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    decls: 0,
    vars: 0,
    template: function BackdropComponent_Template(rf, ctx) {},
    styles: ["[_nghost-%COMP%] {\n  visibility: hidden;\n  opacity: 0;\n  position: fixed;\n  z-index: var(--theme-backdrop-zIndex);\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  right: 0;\n  bottom: 0;\n  top: 0;\n  left: 0;\n  background-color: var(--palette-background-backdrop);\n  transition: opacity var(--theme-anim-duration-leavingScreen) var(--theme-anim-easing-easeInOut) 0ms;\n}\n\n.opened[_nghost-%COMP%] {\n  visibility: visible;\n  opacity: 1;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9iYWNrZHJvcC9iYWNrZHJvcC5jb21wb25lbnQuc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL21peGlucy9fZmxleC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUVBO0VBQ0Usa0JBQUE7RUFDQSxVQUFBO0VBQ0EsZUFBQTtFQUNBLHFDQUFBO0VDWUEsYUFBQTtFQUNBLG1CQUFBO0VBQ0EsdUJBQUE7RURaQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLE1BQUE7RUFDQSxPQUFBO0VBQ0Esb0RBQUE7RUFDQSxtR0FBQTtBQUNGOztBQUdBO0VBQ0UsbUJBQUE7RUFDQSxVQUFBO0FBQUYiLCJzb3VyY2VzQ29udGVudCI6WyJAaW1wb3J0ICcuLi8uLi90b2tlbi9taXhpbnMvZmxleCc7XG5cbjpob3N0IHtcbiAgdmlzaWJpbGl0eTogaGlkZGVuO1xuICBvcGFjaXR5OiAwO1xuICBwb3NpdGlvbjogZml4ZWQ7XG4gIHotaW5kZXg6IHZhcigtLXRoZW1lLWJhY2tkcm9wLXpJbmRleCk7XG4gIEBpbmNsdWRlIGQtZmxleC12aDtcbiAgcmlnaHQ6IDA7XG4gIGJvdHRvbTogMDtcbiAgdG9wOiAwO1xuICBsZWZ0OiAwO1xuICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLWJhY2tncm91bmQtYmFja2Ryb3ApO1xuICB0cmFuc2l0aW9uOiBvcGFjaXR5IHZhcigtLXRoZW1lLWFuaW0tZHVyYXRpb24tbGVhdmluZ1NjcmVlbilcbiAgICB2YXIoLS10aGVtZS1hbmltLWVhc2luZy1lYXNlSW5PdXQpIDBtcztcbn1cblxuOmhvc3Qub3BlbmVkIHtcbiAgdmlzaWJpbGl0eTogdmlzaWJsZTtcbiAgb3BhY2l0eTogMTtcbn1cbiIsIkBtaXhpbiBkLWZsZXgge1xuICBkaXNwbGF5OiBmbGV4O1xufVxuQG1peGluIGQtaW5saW5lLWZsZXgge1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbn1cblxuQG1peGluIGQtZmxleC12IHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC1oIHtcbiAgZGlzcGxheTogZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtdmgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cbiJdLCJzb3VyY2VSb290IjoiIn0= */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 662:
/*!*********************************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/dark-mode-toggle/dark-mode-toggle.component.ts ***!
  \*********************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   DarkModeToggleComponent: () => (/* binding */ DarkModeToggleComponent)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);





function DarkModeToggleComponent_input_4_Template(rf, ctx) {
  if (rf & 1) {
    const _r3 = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](0, "input", 6);
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵlistener"]("change", function DarkModeToggleComponent_input_4_Template_input_change_0_listener() {
      const restoredCtx = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵrestoreView"](_r3);
      const isLightTheme_r1 = restoredCtx.$implicit;
      const ctx_r2 = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵresetView"](ctx_r2.setChecked(!isLightTheme_r1));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const isLightTheme_r1 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵproperty"]("checked", !isLightTheme_r1);
  }
}
class DarkModeToggleComponent extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_1__.RxState {
  constructor() {
    super();
    this.isLightTheme$ = this.select('isLightTheme');
    this.document = (0,_angular_core__WEBPACK_IMPORTED_MODULE_0__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_2__.DOCUMENT);
    this.toggleTheme = isLightTheme => {
      if (isLightTheme) {
        this.document.body.classList.remove('dark');
        this.document.body.classList.add('light');
      } else {
        this.document.body.classList.add('dark');
        this.document.body.classList.remove('light');
      }
    };
    this.set({
      isLightTheme: true
    });
    this.hold(this.isLightTheme$, this.toggleTheme);
  }
  setChecked(isLightTheme) {
    this.set({
      isLightTheme
    });
  }
  static #_ = this.ɵfac = function DarkModeToggleComponent_Factory(t) {
    return new (t || DarkModeToggleComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: DarkModeToggleComponent,
    selectors: [["ui-dark-mode-toggle"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵInheritDefinitionFeature"], _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    decls: 9,
    vars: 1,
    consts: [[1, "dark-mode-toggle"], ["aria-label", "Enable dark mode", "type", "button", 1, "light", 3, "click"], [1, "toggle"], ["class", "toggle-track", "type", "checkbox", "id", "dark-mode", 3, "checked", "change", 4, "rxLet"], ["for", "dark-mode", 2, "color", "transparent"], ["aria-label", "Disable dark mode", "type", "button", 1, "dark", 3, "click"], ["type", "checkbox", "id", "dark-mode", 1, "toggle-track", 3, "checked", "change"]],
    template: function DarkModeToggleComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](0, "div", 0)(1, "button", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵlistener"]("click", function DarkModeToggleComponent_Template_button_click_1_listener() {
          return ctx.setChecked(true);
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtext"](2, " \u2600 ");
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](3, "span", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtemplate"](4, DarkModeToggleComponent_input_4_Template, 1, 1, "input", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](5, "label", 4);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtext"](6, " Toggle Switch ");
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]()();
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](7, "button", 5);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵlistener"]("click", function DarkModeToggleComponent_Template_button_click_7_listener() {
          return ctx.setChecked(false);
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtext"](8, " \u263E ");
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]()();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵadvance"](4);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵproperty"]("rxLet", ctx.isLightTheme$);
      }
    },
    dependencies: [_rx_angular_template_let__WEBPACK_IMPORTED_MODULE_3__.RxLet],
    styles: ["[_nghost-%COMP%] {\n  display: flex;\n}\n\n.dark-mode-toggle[_ngcontent-%COMP%] {\n  display: flex;\n}\n\n.dark-mode-toggle[_ngcontent-%COMP%]    > button[_ngcontent-%COMP%] {\n  font-size: 2.125rem;\n  background: none;\n  border: none;\n  line-height: 0;\n  cursor: pointer;\n  color: var(--palette-text-primary);\n  transition: color var(--theme-anim-duration-standard) var(--theme-anim-easing-easeInOut);\n}\n\n.dark-mode-toggle[_ngcontent-%COMP%]    > button[_ngcontent-%COMP%]:focus {\n  outline: none;\n}\n\nbutton.light[_ngcontent-%COMP%] {\n  color: var(--palette-text-disabled);\n}\n\nbutton.dark[_ngcontent-%COMP%] {\n  color: var(--palette-custom-tmdbLightBlue);\n}\n\n.light[_nghost-%COMP%]   button.light[_ngcontent-%COMP%], .light   [_nghost-%COMP%]   button.light[_ngcontent-%COMP%] {\n  color: var(--palette-toggle-light-light);\n}\n.light[_nghost-%COMP%]   button.dark[_ngcontent-%COMP%], .light   [_nghost-%COMP%]   button.dark[_ngcontent-%COMP%] {\n  color: var(--palette-text-primary);\n}\n\n.toggle[_ngcontent-%COMP%] {\n  position: relative;\n  padding: 0 6px;\n  display: flex;\n  align-items: center;\n  width: 45px;\n}\n\ninput[type=checkbox].toggle-track[_ngcontent-%COMP%] {\n  width: 34px;\n  height: 14px;\n  opacity: 0.5;\n  background-color: var(--palette-secondary-main);\n  position: relative;\n  border-radius: 7px;\n  appearance: none;\n  cursor: pointer;\n  vertical-align: 2px;\n  outline: none;\n}\n\ninput[type=checkbox].toggle-track[_ngcontent-%COMP%]:checked    + label[_ngcontent-%COMP%] {\n  left: 20px;\n}\n\ninput[type=checkbox].toggle-track[_ngcontent-%COMP%]:focus-visible {\n  outline: solid 2px white;\n}\n\ninput[type=checkbox].toggle-track[_ngcontent-%COMP%]    + label[_ngcontent-%COMP%] {\n  -webkit-user-select: none;\n          user-select: none;\n  display: inline-block;\n  width: 20px;\n  height: 20px;\n  border-radius: var(--theme-borderRadius-circle);\n  transition: left var(--theme-anim-duration-standard) var(--theme-anim-easing-easeInOut);\n  cursor: pointer;\n  position: absolute;\n  left: 2px;\n  background-color: var(--palette-secondary-main);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9kYXJrLW1vZGUtdG9nZ2xlL2RhcmstbW9kZS10b2dnbGUuY29tcG9uZW50LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFFQTtFQ0RFLGFBQUE7QURDRjs7QUFJQTtFQ0xFLGFBQUE7QURLRjs7QUFJQTtFQUNFLG1CQUFBO0VBQ0EsZ0JBQUE7RUFDQSxZQUFBO0VBQ0EsY0FBQTtFQUNBLGVBQUE7RUFDQSxrQ0FBQTtFQUNBLHdGQUFBO0FBREY7O0FBS0E7RUFDRSxhQUFBO0FBRkY7O0FBS0E7RUFDRSxtQ0FBQTtBQUZGOztBQUtBO0VBQ0UsMENBQUE7QUFGRjs7QUFNRTtFQUNFLHdDQUFBO0FBSEo7QUFNRTtFQUNFLGtDQUFBO0FBSko7O0FBUUE7RUFDRSxrQkFBQTtFQUNBLGNBQUE7RUNyQ0EsYUFBQTtFQUNBLG1CQUFBO0VEc0NBLFdBQUE7QUFKRjs7QUFPQTtFQUNFLFdBQUE7RUFDQSxZQUFBO0VBQ0EsWUFBQTtFQUNBLCtDQUFBO0VBQ0Esa0JBQUE7RUFDQSxrQkFBQTtFQUNBLGdCQUFBO0VBQ0EsZUFBQTtFQUNBLG1CQUFBO0VBQ0EsYUFBQTtBQUpGOztBQU9BO0VBQ0UsVUFBQTtBQUpGOztBQU9BO0VBQ0Usd0JBQUE7QUFKRjs7QUFPQTtFQUNFLHlCQUFBO1VBQUEsaUJBQUE7RUFDQSxxQkFBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0EsK0NBQUE7RUFDQSx1RkFBQTtFQUVBLGVBQUE7RUFDQSxrQkFBQTtFQUNBLFNBQUE7RUFDQSwrQ0FBQTtBQUxGIiwic291cmNlc0NvbnRlbnQiOlsiQGltcG9ydCAnLi4vLi4vdG9rZW4vbWl4aW5zL2ZsZXgnO1xuXG46aG9zdCB7XG4gIEBpbmNsdWRlIGQtZmxleDtcbn1cblxuLmRhcmstbW9kZS10b2dnbGUge1xuICBAaW5jbHVkZSBkLWZsZXg7XG59XG5cbi5kYXJrLW1vZGUtdG9nZ2xlID4gYnV0dG9uIHtcbiAgZm9udC1zaXplOiAyLjEyNXJlbTtcbiAgYmFja2dyb3VuZDogbm9uZTtcbiAgYm9yZGVyOiBub25lO1xuICBsaW5lLWhlaWdodDogMDtcbiAgY3Vyc29yOiBwb2ludGVyO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS10ZXh0LXByaW1hcnkpO1xuICB0cmFuc2l0aW9uOiBjb2xvciB2YXIoLS10aGVtZS1hbmltLWR1cmF0aW9uLXN0YW5kYXJkKVxuICAgIHZhcigtLXRoZW1lLWFuaW0tZWFzaW5nLWVhc2VJbk91dCk7XG59XG5cbi5kYXJrLW1vZGUtdG9nZ2xlID4gYnV0dG9uOmZvY3VzIHtcbiAgb3V0bGluZTogbm9uZTtcbn1cblxuYnV0dG9uLmxpZ2h0IHtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtdGV4dC1kaXNhYmxlZCk7XG59XG5cbmJ1dHRvbi5kYXJrIHtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtY3VzdG9tLXRtZGJMaWdodEJsdWUpO1xufVxuXG46aG9zdC1jb250ZXh0KC5saWdodCkge1xuICBidXR0b24ubGlnaHQge1xuICAgIGNvbG9yOiB2YXIoLS1wYWxldHRlLXRvZ2dsZS1saWdodC1saWdodCk7XG4gIH1cblxuICBidXR0b24uZGFyayB7XG4gICAgY29sb3I6IHZhcigtLXBhbGV0dGUtdGV4dC1wcmltYXJ5KTtcbiAgfVxufVxuXG4udG9nZ2xlIHtcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuICBwYWRkaW5nOiAwIDZweDtcbiAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gIHdpZHRoOiA0NXB4O1xufVxuXG5pbnB1dFt0eXBlPSdjaGVja2JveCddLnRvZ2dsZS10cmFjayB7XG4gIHdpZHRoOiAzNHB4O1xuICBoZWlnaHQ6IDE0cHg7XG4gIG9wYWNpdHk6IDAuNTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gIHBvc2l0aW9uOiByZWxhdGl2ZTtcbiAgYm9yZGVyLXJhZGl1czogN3B4O1xuICBhcHBlYXJhbmNlOiBub25lO1xuICBjdXJzb3I6IHBvaW50ZXI7XG4gIHZlcnRpY2FsLWFsaWduOiAycHg7XG4gIG91dGxpbmU6IG5vbmU7XG59XG5cbmlucHV0W3R5cGU9J2NoZWNrYm94J10udG9nZ2xlLXRyYWNrOmNoZWNrZWQgKyBsYWJlbCB7XG4gIGxlZnQ6IDIwcHg7XG59XG5cbmlucHV0W3R5cGU9J2NoZWNrYm94J10udG9nZ2xlLXRyYWNrOmZvY3VzLXZpc2libGUge1xuICBvdXRsaW5lOiBzb2xpZCAycHggd2hpdGU7XG59XG5cbmlucHV0W3R5cGU9J2NoZWNrYm94J10udG9nZ2xlLXRyYWNrICsgbGFiZWwge1xuICB1c2VyLXNlbGVjdDogbm9uZTtcbiAgZGlzcGxheTogaW5saW5lLWJsb2NrO1xuICB3aWR0aDogMjBweDtcbiAgaGVpZ2h0OiAyMHB4O1xuICBib3JkZXItcmFkaXVzOiB2YXIoLS10aGVtZS1ib3JkZXJSYWRpdXMtY2lyY2xlKTtcbiAgdHJhbnNpdGlvbjogbGVmdCB2YXIoLS10aGVtZS1hbmltLWR1cmF0aW9uLXN0YW5kYXJkKVxuICAgIHZhcigtLXRoZW1lLWFuaW0tZWFzaW5nLWVhc2VJbk91dCk7XG4gIGN1cnNvcjogcG9pbnRlcjtcbiAgcG9zaXRpb246IGFic29sdXRlO1xuICBsZWZ0OiAycHg7XG4gIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtc2Vjb25kYXJ5LW1haW4pO1xufVxuIiwiQG1peGluIGQtZmxleCB7XG4gIGRpc3BsYXk6IGZsZXg7XG59XG5AbWl4aW4gZC1pbmxpbmUtZmxleCB7XG4gIGRpc3BsYXk6IGlubGluZS1mbGV4O1xufVxuXG5AbWl4aW4gZC1mbGV4LXYge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LWgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC12aCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 1703:
/*!*********************************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/hamburger-button/hamburger-button.component.ts ***!
  \*********************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   HamburgerButtonComponent: () => (/* binding */ HamburgerButtonComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);

class HamburgerButtonComponent {
  static #_ = this.ɵfac = function HamburgerButtonComponent_Factory(t) {
    return new (t || HamburgerButtonComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: HamburgerButtonComponent,
    selectors: [["ui-hamburger-button"]],
    hostAttrs: [1, "hamburger-button"],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    decls: 3,
    vars: 0,
    consts: [[1, "bar"]],
    template: function HamburgerButtonComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelement"](0, "span", 0)(1, "span", 0)(2, "span", 0);
      }
    },
    styles: ["[_nghost-%COMP%] {\n  display: flex;\n  flex-direction: column;\n  align-self: center;\n  justify-content: space-around;\n  width: 25px;\n  cursor: pointer;\n}\n\n.bar[_ngcontent-%COMP%] {\n  width: 100%;\n  height: 4px;\n  margin: 2px 0;\n  border-radius: 4px;\n  background-color: var(--palette-secondary-main);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9oYW1idXJnZXItYnV0dG9uL2hhbWJ1cmdlci1idXR0b24uY29tcG9uZW50LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFFQTtFQ0RFLGFBQUE7RURHQSxzQkFBQTtFQUNBLGtCQUFBO0VBQ0EsNkJBQUE7RUFDQSxXQUFBO0VBQ0EsZUFBQTtBQURGOztBQUdBO0VBQ0UsV0FBQTtFQUNBLFdBQUE7RUFDQSxhQUFBO0VBQ0Esa0JBQUE7RUFDQSwrQ0FBQTtBQUFGIiwic291cmNlc0NvbnRlbnQiOlsiQGltcG9ydCAnLi4vLi4vdG9rZW4vbWl4aW5zL2ZsZXgnO1xuXG46aG9zdCB7XG4gIEBpbmNsdWRlIGQtZmxleDtcbiAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbiAgYWxpZ24tc2VsZjogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWFyb3VuZDtcbiAgd2lkdGg6IDI1cHg7XG4gIGN1cnNvcjogcG9pbnRlcjtcbn1cbi5iYXIge1xuICB3aWR0aDogMTAwJTtcbiAgaGVpZ2h0OiA0cHg7XG4gIG1hcmdpbjogMnB4IDA7XG4gIGJvcmRlci1yYWRpdXM6IDRweDtcbiAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 2152:
/*!*********************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/search-bar/search-bar.component.ts ***!
  \*********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   SearchBarComponent: () => (/* binding */ SearchBarComponent)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ 9016);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 8989);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! rxjs */ 1527);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! rxjs */ 5043);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! rxjs */ 7835);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _rx_angular_cdk_coercing__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @rx-angular/cdk/coercing */ 1509);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);











const _c0 = ["searchInput"];
const _c1 = ["form"];
function SearchBarComponent_input_4_Template(rf, ctx) {
  if (rf & 1) {
    const _r5 = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](0, "input", 5, 6);
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵlistener"]("change", function SearchBarComponent_input_4_Template_input_change_0_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵrestoreView"](_r5);
      const _r3 = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵreference"](1);
      const ctx_r4 = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵresetView"](ctx_r4.ui.searchChange(_r3.value));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const search_r2 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵproperty"]("value", search_r2);
  }
}
class SearchBarComponent {
  set query(v) {
    // eslint-disable-next-line @rx-angular/no-rxstate-subscriptions-outside-constructor
    this.state.connect('search', (0,_rx_angular_cdk_coercing__WEBPACK_IMPORTED_MODULE_1__.coerceObservable)(v));
  }
  outsideClick() {
    // any click on the page (we can't use the option `once:true` as we might get multiple false trigger)
    return (0,rxjs__WEBPACK_IMPORTED_MODULE_2__.fromEvent)(this.document, 'click').pipe(
    // forward if the form did NOT trigger the click
    // means we clicked somewhere else in the page but the form
    (0,rxjs__WEBPACK_IMPORTED_MODULE_3__.filter)(e => !this.formRef.nativeElement.contains(e.target)));
  }
  constructor(state, actions) {
    this.state = state;
    this.actions = actions;
    this.document = (0,_angular_core__WEBPACK_IMPORTED_MODULE_0__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_4__.DOCUMENT);
    this.elementRef = (0,_angular_core__WEBPACK_IMPORTED_MODULE_0__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_0__.ElementRef);
    this.ui = this.actions.create({
      searchChange: String,
      formSubmit: _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_5__.preventDefault
    });
    this.search$ = this.state.select('search');
    this.searchSubmit = this.ui.formSubmit$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.withLatestFrom)(this.state.select('search')), (0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(([, search]) => search));
    this.closedFormClick$ = this.ui.formClick$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.withLatestFrom)(this.state.select('open')), (0,rxjs__WEBPACK_IMPORTED_MODULE_3__.filter)(([, opened]) => !opened));
    /**
     * **🚀 Perf Tip for TBT, TTI:**
     *
     * We avoid `@HostListener('document')` as it would add an event listener on component bootstrap no matter if we need it or not.
     * This obviously will not scale.
     *
     * To avoid this we only listen to document click events after we clicked on the closed form.
     * If the needed event to close the form is received we stop listening to the document.
     *
     * This way we reduce the active event listeners to a minimum.
     */
    this.outsideOpenFormClick$ = this.closedFormClick$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.switchMap)(() => this.outsideClick().pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_9__.take)(1))));
    this.classList = this.elementRef.nativeElement.classList;
    this.focusInput = () => {
      // eslint-disable-next-line @rx-angular/prefer-no-layout-sensitive-apis
      return this.inputRef.nativeElement.focus();
    };
    this.setOpenedStyling = opened => {
      opened ? this.classList.add('opened') : this.classList.remove('opened');
    };
    this.state.set({
      open: false
    });
    this.state.connect('search', this.ui.searchChange$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_10__.startWith)('')));
    this.state.connect('open', (0,rxjs__WEBPACK_IMPORTED_MODULE_11__.merge)(this.ui.formSubmit$, this.outsideOpenFormClick$), () => false);
    this.state.connect('open', this.closedFormClick$, () => true);
    this.state.hold(this.state.select('open'), this.setOpenedStyling);
    this.state.hold(this.closedFormClick$, this.focusInput);
  }
  static #_ = this.ɵfac = function SearchBarComponent_Factory(t) {
    return new (t || SearchBarComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdirectiveInject"](_rx_angular_state__WEBPACK_IMPORTED_MODULE_12__.RxState), _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_5__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: SearchBarComponent,
    selectors: [["ui-search-bar"]],
    viewQuery: function SearchBarComponent_Query(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵviewQuery"](_c0, 5);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵviewQuery"](_c1, 5);
      }
      if (rf & 2) {
        let _t;
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵqueryRefresh"](_t = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵloadQuery"]()) && (ctx.inputRef = _t.first);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵqueryRefresh"](_t = _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵloadQuery"]()) && (ctx.formRef = _t.first);
      }
    },
    inputs: {
      query: "query"
    },
    outputs: {
      searchSubmit: "searchSubmit"
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵProvidersFeature"]([_rx_angular_state__WEBPACK_IMPORTED_MODULE_12__.RxState]), _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    decls: 5,
    vars: 2,
    consts: [["data-uf", "q-form", 1, "form", 3, "tabIndex", "submit", "focus"], ["form", ""], ["type", "submit", "aria-label", "Search for a movie", 1, "magnifier-button"], ["name", "search", "size", "1.125em"], ["data-uf", "q", "aria-label", "Search Input", "placeholder", "Search for a movie...", "class", "input", 3, "value", "change", 4, "rxLet"], ["data-uf", "q", "aria-label", "Search Input", "placeholder", "Search for a movie...", 1, "input", 3, "value", "change"], ["searchInput", ""]],
    template: function SearchBarComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](0, "form", 0, 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵlistener"]("submit", function SearchBarComponent_Template_form_submit_0_listener($event) {
          return ctx.ui.formSubmit($event);
        })("focus", function SearchBarComponent_Template_form_focus_0_listener($event) {
          return ctx.ui.formClick($event);
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementStart"](2, "button", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelement"](3, "fast-svg", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵtemplate"](4, SearchBarComponent_input_4_Template, 2, 1, "input", 4);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵproperty"]("tabIndex", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵadvance"](4);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵproperty"]("rxLet", ctx.search$);
      }
    },
    dependencies: [_rx_angular_template_let__WEBPACK_IMPORTED_MODULE_13__.RxLet, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_14__.FastSvgComponent],
    styles: ["[_nghost-%COMP%] {\n  display: contents;\n}\n\n.form[_ngcontent-%COMP%] {\n  position: relative;\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  box-shadow: var(--theme-shadow-1);\n  background-color: var(--palette-secondary-dark);\n  border: 1px solid var(--palette-secondary-main);\n  width: 2rem;\n  cursor: pointer;\n  padding: 2rem;\n  height: 2rem;\n  outline: none;\n  border-radius: 100px;\n  contain: strict;\n  transform: translateZ(0px);\n  will-change: width;\n  transition: width var(--theme-anim-duration-standard) var(--theme-anim-easing-easeInOut);\n}\n\n.magnifier-button[_ngcontent-%COMP%] {\n  line-height: 0;\n  pointer-events: none;\n  cursor: none;\n  background-color: transparent;\n  border: none;\n  outline: none;\n  color: var(--palette-secondary-contrast-text);\n}\n\n.input[_ngcontent-%COMP%] {\n  font-size: var(--text-md);\n  font-weight: 300;\n  background-color: transparent;\n  width: 100%;\n  margin-left: 0;\n  color: var(--palette-secondary-contrast-text);\n  border: none;\n  transition: margin-left var(--theme-anim-duration-standard) var(--theme-anim-easing-easeInOut);\n}\n\ninput[_ngcontent-%COMP%]:focus, input[_ngcontent-%COMP%]:active {\n  outline: none;\n}\n\ninput[_ngcontent-%COMP%]::placeholder {\n  color: var(--palette-secondary-contrast-text);\n}\n\n.opened[_nghost-%COMP%]   .form[_ngcontent-%COMP%] {\n  width: 30rem;\n  cursor: auto;\n}\n\n.opened[_nghost-%COMP%]   .magnifier-button[_ngcontent-%COMP%] {\n  pointer-events: auto;\n  cursor: pointer;\n}\n\n.opened[_nghost-%COMP%]   .input[_ngcontent-%COMP%] {\n  margin-left: 1rem;\n}\n\n@media only screen and (max-width: 1300px) {\n  .magnifier-button[_ngcontent-%COMP%] {\n    font-size: 1rem;\n  }\n  .input[_ngcontent-%COMP%] {\n    font-size: 1.25rem;\n  }\n  .form[_ngcontent-%COMP%] {\n    padding: 1.5rem;\n    border: 1px solid hsla(0, 0%, 0%, 0);\n    background-color: var(--palette-secondary-main);\n  }\n}\n@media only screen and (max-width: 900px) {\n  .input[_ngcontent-%COMP%] {\n    font-size: 1rem;\n  }\n}\n@media only screen and (max-width: 600px) {\n  .input[_ngcontent-%COMP%] {\n    font-size: 0.875rem;\n  }\n}\n@media only screen and (max-width: 500px) {\n  .form[_ngcontent-%COMP%] {\n    max-width: 16rem;\n  }\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9zZWFyY2gtYmFyL3NlYXJjaC1iYXIuY29tcG9uZW50LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFFQTtFQUNFLGlCQUFBO0FBREY7O0FBSUE7RUFDRSxrQkFBQTtFQ1dBLGFBQUE7RUFDQSxtQkFBQTtFQUNBLHVCQUFBO0VEWEEsaUNBQUE7RUFDQSwrQ0FBQTtFQUNBLCtDQUFBO0VBQ0EsV0FBQTtFQUNBLGVBQUE7RUFDQSxhQUFBO0VBQ0EsWUFBQTtFQUNBLGFBQUE7RUFDQSxvQkFBQTtFQUNBLGVBQUE7RUFDQSwwQkFBQTtFQUNBLGtCQUFBO0VBQ0Esd0ZBQUE7QUFDRjs7QUFHQTtFQUNFLGNBQUE7RUFDQSxvQkFBQTtFQUNBLFlBQUE7RUFDQSw2QkFBQTtFQUNBLFlBQUE7RUFDQSxhQUFBO0VBQ0EsNkNBQUE7QUFBRjs7QUFHQTtFQUNFLHlCQUFBO0VBQ0EsZ0JBQUE7RUFDQSw2QkFBQTtFQUNBLFdBQUE7RUFDQSxjQUFBO0VBQ0EsNkNBQUE7RUFDQSxZQUFBO0VBQ0EsOEZBQUE7QUFBRjs7QUFJQTs7RUFFRSxhQUFBO0FBREY7O0FBSUE7RUFDRSw2Q0FBQTtBQURGOztBQUlBO0VBQ0UsWUFBQTtFQUNBLFlBQUE7QUFERjs7QUFJQTtFQUNFLG9CQUFBO0VBQ0EsZUFBQTtBQURGOztBQUlBO0VBQ0UsaUJBQUE7QUFERjs7QUFLQTtFQUNFO0lBQ0UsZUFBQTtFQUZGO0VBS0E7SUFDRSxrQkFBQTtFQUhGO0VBS0E7SUFDRSxlQUFBO0lBQ0Esb0NBQUE7SUFDQSwrQ0FBQTtFQUhGO0FBQ0Y7QUFNQTtFQUNFO0lBQ0UsZUFBQTtFQUpGO0FBQ0Y7QUFPQTtFQUNFO0lBQ0UsbUJBQUE7RUFMRjtBQUNGO0FBUUE7RUFDRTtJQUNFLGdCQUFBO0VBTkY7QUFDRiIsInNvdXJjZXNDb250ZW50IjpbIkBpbXBvcnQgJy4uLy4uL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICBkaXNwbGF5OiBjb250ZW50cztcbn1cblxuLmZvcm0ge1xuICBwb3NpdGlvbjogcmVsYXRpdmU7XG4gIEBpbmNsdWRlIGQtZmxleC12aDtcbiAgYm94LXNoYWRvdzogdmFyKC0tdGhlbWUtc2hhZG93LTEpO1xuICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXNlY29uZGFyeS1kYXJrKTtcbiAgYm9yZGVyOiAxcHggc29saWQgdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gIHdpZHRoOiAycmVtO1xuICBjdXJzb3I6IHBvaW50ZXI7XG4gIHBhZGRpbmc6IDJyZW07XG4gIGhlaWdodDogMnJlbTtcbiAgb3V0bGluZTogbm9uZTtcbiAgYm9yZGVyLXJhZGl1czogMTAwcHg7XG4gIGNvbnRhaW46IHN0cmljdDtcbiAgdHJhbnNmb3JtOiB0cmFuc2xhdGVaKDBweCk7XG4gIHdpbGwtY2hhbmdlOiB3aWR0aDtcbiAgdHJhbnNpdGlvbjogd2lkdGggdmFyKC0tdGhlbWUtYW5pbS1kdXJhdGlvbi1zdGFuZGFyZClcbiAgICB2YXIoLS10aGVtZS1hbmltLWVhc2luZy1lYXNlSW5PdXQpO1xufVxuXG4ubWFnbmlmaWVyLWJ1dHRvbiB7XG4gIGxpbmUtaGVpZ2h0OiAwO1xuICBwb2ludGVyLWV2ZW50czogbm9uZTtcbiAgY3Vyc29yOiBub25lO1xuICBiYWNrZ3JvdW5kLWNvbG9yOiB0cmFuc3BhcmVudDtcbiAgYm9yZGVyOiBub25lO1xuICBvdXRsaW5lOiBub25lO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktY29udHJhc3QtdGV4dCk7XG59XG5cbi5pbnB1dCB7XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1tZCk7XG4gIGZvbnQtd2VpZ2h0OiAzMDA7XG4gIGJhY2tncm91bmQtY29sb3I6IHRyYW5zcGFyZW50O1xuICB3aWR0aDogMTAwJTtcbiAgbWFyZ2luLWxlZnQ6IDA7XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXNlY29uZGFyeS1jb250cmFzdC10ZXh0KTtcbiAgYm9yZGVyOiBub25lO1xuICB0cmFuc2l0aW9uOiBtYXJnaW4tbGVmdCB2YXIoLS10aGVtZS1hbmltLWR1cmF0aW9uLXN0YW5kYXJkKVxuICAgIHZhcigtLXRoZW1lLWFuaW0tZWFzaW5nLWVhc2VJbk91dCk7XG59XG5cbmlucHV0OmZvY3VzLFxuaW5wdXQ6YWN0aXZlIHtcbiAgb3V0bGluZTogbm9uZTtcbn1cblxuaW5wdXQ6OnBsYWNlaG9sZGVyIHtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtc2Vjb25kYXJ5LWNvbnRyYXN0LXRleHQpO1xufVxuXG46aG9zdC5vcGVuZWQgLmZvcm0ge1xuICB3aWR0aDogMzByZW07XG4gIGN1cnNvcjogYXV0bztcbn1cblxuOmhvc3Qub3BlbmVkIC5tYWduaWZpZXItYnV0dG9uIHtcbiAgcG9pbnRlci1ldmVudHM6IGF1dG87XG4gIGN1cnNvcjogcG9pbnRlcjtcbn1cblxuOmhvc3Qub3BlbmVkIC5pbnB1dCB7XG4gIG1hcmdpbi1sZWZ0OiAxcmVtO1xuICAvL2N1cnNvcjogcG9pbnRlcjtcbn1cblxuQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiAxMzAwcHgpIHtcbiAgLm1hZ25pZmllci1idXR0b24ge1xuICAgIGZvbnQtc2l6ZTogMXJlbTtcbiAgfVxuXG4gIC5pbnB1dCB7XG4gICAgZm9udC1zaXplOiAxLjI1cmVtO1xuICB9XG4gIC5mb3JtIHtcbiAgICBwYWRkaW5nOiAxLjVyZW07XG4gICAgYm9yZGVyOiAxcHggc29saWQgaHNsKDBkZWcgMCUgMCUgLyAwJSk7XG4gICAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1zZWNvbmRhcnktbWFpbik7XG4gIH1cbn1cblxuQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA5MDBweCkge1xuICAuaW5wdXQge1xuICAgIGZvbnQtc2l6ZTogMXJlbTtcbiAgfVxufVxuXG5AbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDYwMHB4KSB7XG4gIC5pbnB1dCB7XG4gICAgZm9udC1zaXplOiAwLjg3NXJlbTtcbiAgfVxufVxuXG5AbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDUwMHB4KSB7XG4gIC5mb3JtIHtcbiAgICBtYXgtd2lkdGg6IDE2cmVtO1xuICB9XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 9707:
/*!***********************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/side-drawer/side-drawer.component.ts ***!
  \***********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   SideDrawerComponent: () => (/* binding */ SideDrawerComponent)
/* harmony export */ });
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _backdrop_backdrop_component__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../backdrop/backdrop.component */ 4788);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);




const _c0 = ["*"];
class SideDrawerComponent {
  constructor(actions) {
    this.actions = actions;
    this.ui = this.actions.create();
    this.opened = false;
    this.openedChange = this.ui.openedChange$;
  }
  static #_ = this.ɵfac = function SideDrawerComponent_Factory(t) {
    return new (t || SideDrawerComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_2__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineComponent"]({
    type: SideDrawerComponent,
    selectors: [["ui-side-drawer"]],
    inputs: {
      opened: "opened"
    },
    outputs: {
      openedChange: "openedChange"
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵProvidersFeature"]([_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_2__.RxActionFactory]), _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵStandaloneFeature"]],
    ngContentSelectors: _c0,
    decls: 3,
    vars: 3,
    consts: [[3, "opened", "click"], [1, "side-drawer"]],
    template: function SideDrawerComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵprojectionDef"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](0, "ui-backdrop", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵlistener"]("click", function SideDrawerComponent_Template_ui_backdrop_click_0_listener() {
          return ctx.ui.openedChange(false);
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](1, "div", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵprojection"](2);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("opened", ctx.opened);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵclassProp"]("opened", ctx.opened);
      }
    },
    dependencies: [_backdrop_backdrop_component__WEBPACK_IMPORTED_MODULE_0__.BackdropComponent],
    styles: ["[_nghost-%COMP%] {\n  display: contents;\n}\n\n.side-drawer[_ngcontent-%COMP%] {\n  contain: strict;\n  content-visibility: auto;\n  position: fixed;\n  display: flex;\n  flex-direction: column;\n  justify-content: space-between;\n  width: 250px;\n  max-width: 70%;\n  height: 100%;\n  left: 0;\n  top: 0;\n  z-index: var(--theme-drawer-zIndex);\n  padding: 32px 16px 16px;\n  overflow-y: auto;\n  box-sizing: border-box;\n  transition: transform var(--theme-anim-duration-short) var(--theme-anim-easing-easeOut);\n  box-shadow: var(--theme-shadow-16);\n  background-color: var(--palette-background-paper);\n  transform: translateX(-100%);\n}\n.side-drawer[_ngcontent-%COMP%]    + footer[_ngcontent-%COMP%] {\n  transition: transform var(--theme-anim-duration-short) var(--theme-anim-easing-easeOut);\n  transform: translateX(-100%);\n}\n@media screen and (min-width: 1298px) {\n  .side-drawer[_ngcontent-%COMP%] {\n    box-shadow: unset;\n    background-color: transparent;\n  }\n}\n\n@media screen and (min-width: 1298px) {\n  .side-drawer[_ngcontent-%COMP%] {\n    transform: translateX(0);\n  }\n  ui-backdrop[_ngcontent-%COMP%] {\n    display: none;\n  }\n}\n.side-drawer.opened[_ngcontent-%COMP%] {\n  transform: translateX(0);\n}\n.side-drawer.opened[_ngcontent-%COMP%]    + footer[_ngcontent-%COMP%] {\n  transform: translateX(0);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9zaWRlLWRyYXdlci9zaWRlLWRyYXdlci5jb21wb25lbnQuc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL21peGlucy9fZmxleC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL21lZGlhLnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBR0E7RUFDRSxpQkFBQTtBQUZGOztBQUlBO0VBQ0UsZUFBQTtFQUNBLHdCQUFBO0VBQ0EsZUFBQTtFQ1JBLGFBQUE7RURVQSxzQkFBQTtFQUNBLDhCQUFBO0VBQ0EsWUFBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsT0FBQTtFQUNBLE1BQUE7RUFDQSxtQ0FBQTtFQUNBLHVCQUFBO0VBQ0EsZ0JBQUE7RUFDQSxzQkFBQTtFQUNBLHVGQUFBO0VBRUEsa0NBQUE7RUFDQSxpREFBQTtFQUNBLDRCQUFBO0FBRkY7QUFJRTtFQUNFLHVGQUFBO0VBRUEsNEJBQUE7QUFISjtBRVRFO0VGYkY7SUE2QkksaUJBQUE7SUFDQSw2QkFBQTtFQUhGO0FBQ0Y7O0FBTUE7RUFDRTtJQUNFLHdCQUFBO0VBSEY7RUFLQTtJQUNFLGFBQUE7RUFIRjtBQUNGO0FBS0E7RUFDRSx3QkFBQTtBQUhGO0FBS0U7RUFDRSx3QkFBQTtBQUhKIiwic291cmNlc0NvbnRlbnQiOlsiQGltcG9ydCAnLi4vLi4vdG9rZW4vbWl4aW5zL21lZGlhJztcbkBpbXBvcnQgJy4uLy4uL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICBkaXNwbGF5OiBjb250ZW50cztcbn1cbi5zaWRlLWRyYXdlciB7XG4gIGNvbnRhaW46IHN0cmljdDtcbiAgY29udGVudC12aXNpYmlsaXR5OiBhdXRvO1xuICBwb3NpdGlvbjogZml4ZWQ7XG4gIEBpbmNsdWRlIGQtZmxleDtcbiAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbiAganVzdGlmeS1jb250ZW50OiBzcGFjZS1iZXR3ZWVuO1xuICB3aWR0aDogMjUwcHg7XG4gIG1heC13aWR0aDogNzAlO1xuICBoZWlnaHQ6IDEwMCU7XG4gIGxlZnQ6IDA7XG4gIHRvcDogMDtcbiAgei1pbmRleDogdmFyKC0tdGhlbWUtZHJhd2VyLXpJbmRleCk7XG4gIHBhZGRpbmc6IDMycHggMTZweCAxNnB4O1xuICBvdmVyZmxvdy15OiBhdXRvO1xuICBib3gtc2l6aW5nOiBib3JkZXItYm94O1xuICB0cmFuc2l0aW9uOiB0cmFuc2Zvcm0gdmFyKC0tdGhlbWUtYW5pbS1kdXJhdGlvbi1zaG9ydClcbiAgICB2YXIoLS10aGVtZS1hbmltLWVhc2luZy1lYXNlT3V0KTtcbiAgYm94LXNoYWRvdzogdmFyKC0tdGhlbWUtc2hhZG93LTE2KTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1iYWNrZ3JvdW5kLXBhcGVyKTtcbiAgdHJhbnNmb3JtOiB0cmFuc2xhdGVYKC0xMDAlKTtcblxuICAmICsgZm9vdGVyIHtcbiAgICB0cmFuc2l0aW9uOiB0cmFuc2Zvcm0gdmFyKC0tdGhlbWUtYW5pbS1kdXJhdGlvbi1zaG9ydClcbiAgICAgIHZhcigtLXRoZW1lLWFuaW0tZWFzaW5nLWVhc2VPdXQpO1xuICAgIHRyYW5zZm9ybTogdHJhbnNsYXRlWCgtMTAwJSk7XG4gIH1cblxuICBAaW5jbHVkZSBpc0Rlc2t0b3Age1xuICAgIGJveC1zaGFkb3c6IHVuc2V0O1xuICAgIGJhY2tncm91bmQtY29sb3I6IHRyYW5zcGFyZW50O1xuICB9XG59XG5cbkBtZWRpYSBzY3JlZW4gYW5kIChtaW4td2lkdGg6IDEyOThweCkge1xuICAuc2lkZS1kcmF3ZXIge1xuICAgIHRyYW5zZm9ybTogdHJhbnNsYXRlWCgwKTtcbiAgfVxuICB1aS1iYWNrZHJvcCB7XG4gICAgZGlzcGxheTogbm9uZTtcbiAgfVxufVxuLnNpZGUtZHJhd2VyLm9wZW5lZCB7XG4gIHRyYW5zZm9ybTogdHJhbnNsYXRlWCgwKTtcblxuICAmICsgZm9vdGVyIHtcbiAgICB0cmFuc2Zvcm06IHRyYW5zbGF0ZVgoMCk7XG4gIH1cbn1cbiIsIkBtaXhpbiBkLWZsZXgge1xuICBkaXNwbGF5OiBmbGV4O1xufVxuQG1peGluIGQtaW5saW5lLWZsZXgge1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbn1cblxuQG1peGluIGQtZmxleC12IHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC1oIHtcbiAgZGlzcGxheTogZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtdmgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cbiIsIkBtaXhpbiBpc1hTbWFsbCB7XG4gIEBtZWRpYSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDUwMHB4KSB7XG4gICAgQGNvbnRlbnQ7XG4gIH1cbn1cblxuQG1peGluIGlzU21hbGwge1xuICBAbWVkaWEgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA2MDBweCkge1xuICAgIEBjb250ZW50O1xuICB9XG59XG5cbkBtaXhpbiBpc01vYmlsZSB7XG4gIEBtZWRpYSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDEyOTlweCkge1xuICAgIEBjb250ZW50O1xuICB9XG59XG5cbkBtaXhpbiBpc0Rlc2t0b3Age1xuICBAbWVkaWEgc2NyZWVuIGFuZCAobWluLXdpZHRoOiAxMjk4cHgpIHtcbiAgICBAY29udGVudDtcbiAgfVxufVxuXG5AbWl4aW4gaXNMYXJnZURlc2t0b3Age1xuICBAbWVkaWEgc2NyZWVuIGFuZCAobWluLXdpZHRoOiAxMjk4cHgpIHtcbiAgICBAY29udGVudDtcbiAgfVxufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 6136:
/*!*********************************************************!*\
  !*** ./projects/movies/src/environments/environment.ts ***!
  \*********************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   environment: () => (/* binding */ environment)
/* harmony export */ });
// The file contents for the current environment will overwrite these during build.
// The build system defaults to the dev environment which uses `environment.ts`, but if you do
// `ng build --env=prod` then `environment.prod.ts` will be used instead.
// The list of which env maps to which file can be found in `.angular-cli.json`.
const environment = {
  production: false,
  tmdbBaseUrl: 'https://api.themoviedb.org',
  apiV3: '3',
  apiV4: '4',
  tmdbApiKey: '3cfc6e1dd231bd1f2caa198e7317a6a4',
  tmdbApiReadAccessKey: 'eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIzY2ZjNmUxZGQyMzFiZDFmMmNhYTE5OGU3MzE3YTZhNCIsInN1YiI6IjYwZWZiOTZlYTQ0ZDA5MDAyZDQ0ZjNlNSIsInNjb3BlcyI6WyJhcGlfcmVhZCJdLCJ2ZXJzaW9uIjoxfQ.nvvleDHS5FWTK9UbhKfeuW8L5w4hyjGHAphNtQJuYSY'
};

/***/ }),

/***/ 4379:
/*!*************************************!*\
  !*** ./projects/movies/src/main.ts ***!
  \*************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony import */ var _angular_platform_browser__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/platform-browser */ 6480);
/* harmony import */ var _app_app_module__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./app/app.module */ 8031);


/**
 * We have to use `platformBrowserDynamic` until Angular allows to run `bootstrapApplication` in zone-less mode
 */
_angular_platform_browser__WEBPACK_IMPORTED_MODULE_1__.platformBrowser().bootstrapModule(_app_app_module__WEBPACK_IMPORTED_MODULE_0__.AppModule, {
  ngZone: 'noop'
}).catch(err => console.error(err));

/***/ })

},
/******/ __webpack_require__ => { // webpackRuntimeModules
/******/ var __webpack_exec__ = (moduleId) => (__webpack_require__(__webpack_require__.s = moduleId))
/******/ __webpack_require__.O(0, ["vendor"], () => (__webpack_exec__(4379)));
/******/ var __webpack_exports__ = __webpack_require__.O();
/******/ }
]);
//# sourceMappingURL=main.js.map