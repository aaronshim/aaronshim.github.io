"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["default-projects_movies_src_app_ui_pattern_movie-list_movie-list_component_ts"],{

/***/ 2625:
/*!***********************************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/element-visibility/element-visibility.directive.ts ***!
  \***********************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ElementVisibilityDirective: () => (/* binding */ ElementVisibilityDirective)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _observe_element_visibility__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./observe-element-visibility */ 7993);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 274);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);






class ElementVisibilityDirective {
  constructor(actionsF, elRef) {
    this.actionsF = actionsF;
    this.platformId = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_1__.PLATFORM_ID);
    this.signals = this.actionsF.create();
    this.elementVisibility = this.signals.visible$;
    if ((0,_angular_common__WEBPACK_IMPORTED_MODULE_2__.isPlatformBrowser)(this.platformId)) {
      (0,_observe_element_visibility__WEBPACK_IMPORTED_MODULE_0__.observeElementVisibility)(elRef.nativeElement).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_3__.takeUntil)(this.signals.onDestroy$)).subscribe(this.signals.visible);
    }
  }
  ngOnDestroy() {
    this.signals.onDestroy();
  }
  static #_ = this.ɵfac = function ElementVisibilityDirective_Factory(t) {
    return new (t || ElementVisibilityDirective)(_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_4__.RxActionFactory), _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdirectiveInject"](_angular_core__WEBPACK_IMPORTED_MODULE_1__.ElementRef));
  };
  static #_2 = this.ɵdir = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineDirective"]({
    type: ElementVisibilityDirective,
    selectors: [["", "elementVisibility", ""]],
    outputs: {
      elementVisibility: "elementVisibility"
    },
    standalone: true
  });
}


/***/ }),

/***/ 7993:
/*!*********************************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/element-visibility/observe-element-visibility.ts ***!
  \*********************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   observeElementVisibility: () => (/* binding */ observeElementVisibility)
/* harmony export */ });
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! rxjs */ 2235);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! rxjs */ 3317);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! rxjs */ 2568);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! rxjs */ 274);

function observeElementVisibility(element, cfg) {
  const {
    stop$,
    ..._cgf
  } = cfg || {};
  return new rxjs__WEBPACK_IMPORTED_MODULE_0__.Observable(subscriber => {
    const observer = new IntersectionObserver(entries => {
      subscriber.next(entries[0].isIntersecting);
    }, {
      root: null,
      rootMargin: '500px',
      threshold: 0.5,
      ..._cgf
    });
    observer.observe(element);
    return () => observer.disconnect();
  }).pipe(
  // only forward changes in visibility
  (0,rxjs__WEBPACK_IMPORTED_MODULE_1__.distinctUntilChanged)(), (0,rxjs__WEBPACK_IMPORTED_MODULE_2__.isObservable)(stop$) ? (0,rxjs__WEBPACK_IMPORTED_MODULE_3__.takeUntil)(stop$) : o => o);
}

/***/ }),

/***/ 7866:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/grid-list/grid-list.component.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   GridListComponent: () => (/* binding */ GridListComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);

const _c0 = [[["", 8, "ui-grid-list-item"]], "*"];
const _c1 = [".ui-grid-list-item", "*"];
class GridListComponent {
  static #_ = this.ɵfac = function GridListComponent_Factory(t) {
    return new (t || GridListComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: GridListComponent,
    selectors: [["ui-grid-list"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    ngContentSelectors: _c1,
    decls: 2,
    vars: 0,
    template: function GridListComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojectionDef"](_c0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](1, 1);
      }
    },
    styles: ["[_nghost-%COMP%] {\n  display: grid;\n  grid-template-columns: repeat(auto-fit, minmax(10rem, 35rem));\n  gap: 4rem 2rem;\n  place-content: space-between space-evenly;\n  align-items: start;\n  margin: 4rem 0;\n  position: relative;\n  contain: layout;\n}\n@media only screen and (max-width: 600px) {\n  [_nghost-%COMP%] {\n    grid-template-columns: repeat(auto-fit, minmax(10rem, 23rem));\n    justify-content: space-around;\n    gap: 4rem 1.5rem;\n  }\n}\n@media only screen and (max-width: 500px) {\n  [_nghost-%COMP%] {\n    grid-template-columns: repeat(auto-fit, minmax(10rem, 18rem));\n    gap: 4rem 1rem;\n  }\n}\n[_nghost-%COMP%]  .ui-grid-list-item {\n  display: flex;\n  align-items: stretch;\n  flex-direction: column;\n  transition: transform 150ms cubic-bezier(0.4, 0, 0.2, 1) 0s;\n}\n[_nghost-%COMP%]  .ui-grid-list-item:after {\n  content: \"\";\n  position: absolute;\n  z-index: -99;\n  top: 0px;\n  left: 0px;\n  width: 100%;\n  height: 100%;\n  transform: scaleY(0);\n  transform-origin: center top;\n  background-color: var(--palette-background-paper);\n  box-shadow: rgba(0, 0, 0, 0.2) 0 2px 1px -1px, rgba(0, 0, 0, 0.14) 0px 1px 1px 0px, rgba(0, 0, 0, 0.12) 0px 1px 3px 0px;\n}\n[_nghost-%COMP%]  .ui-grid-list-item:hover {\n  transform: scale(1.03);\n}\n[_nghost-%COMP%]  .ui-grid-list-item:hover:after {\n  transform: scale(1);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9ncmlkLWxpc3QvZ3JpZC1saXN0LmNvbXBvbmVudC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL19mbGV4LnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBRUE7RUFDRSxhQUFBO0VBQ0EsNkRBQUE7RUFDQSxjQUFBO0VBQ0EseUNBQUE7RUFDQSxrQkFBQTtFQUNBLGNBQUE7RUFDQSxrQkFBQTtFQUNBLGVBQUE7QUFERjtBQUdFO0VBVkY7SUFXSSw2REFBQTtJQUNBLDZCQUFBO0lBQ0EsZ0JBQUE7RUFBRjtBQUNGO0FBRUU7RUFoQkY7SUFpQkksNkRBQUE7SUFDQSxjQUFBO0VBQ0Y7QUFDRjtBQUVJO0VDdkJGLGFBQUE7RUR5Qkksb0JBQUE7RUFDQSxzQkFBQTtFQUNBLDJEQUFBO0FBQU47QUFFTTtFQUNFLFdBQUE7RUFDQSxrQkFBQTtFQUNBLFlBQUE7RUFDQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0Esb0JBQUE7RUFDQSw0QkFBQTtFQUNBLGlEQUFBO0VBQ0EsdUhBQUE7QUFBUjtBQUlNO0VBQ0Usc0JBQUE7QUFGUjtBQUlRO0VBQ0UsbUJBQUE7QUFGViIsInNvdXJjZXNDb250ZW50IjpbIkBpbXBvcnQgJy4uLy4uLy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICBkaXNwbGF5OiBncmlkO1xuICBncmlkLXRlbXBsYXRlLWNvbHVtbnM6IHJlcGVhdChhdXRvLWZpdCwgbWlubWF4KDEwcmVtLCAzNXJlbSkpO1xuICBnYXA6IDRyZW0gMnJlbTtcbiAgcGxhY2UtY29udGVudDogc3BhY2UtYmV0d2VlbiBzcGFjZS1ldmVubHk7XG4gIGFsaWduLWl0ZW1zOiBzdGFydDtcbiAgbWFyZ2luOiA0cmVtIDA7XG4gIHBvc2l0aW9uOiByZWxhdGl2ZTtcbiAgY29udGFpbjogbGF5b3V0O1xuXG4gIEBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogNjAwcHgpIHtcbiAgICBncmlkLXRlbXBsYXRlLWNvbHVtbnM6IHJlcGVhdChhdXRvLWZpdCwgbWlubWF4KDEwcmVtLCAyM3JlbSkpO1xuICAgIGp1c3RpZnktY29udGVudDogc3BhY2UtYXJvdW5kO1xuICAgIGdhcDogNHJlbSAxLjVyZW07XG4gIH1cblxuICBAbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDUwMHB4KSB7XG4gICAgZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zOiByZXBlYXQoYXV0by1maXQsIG1pbm1heCgxMHJlbSwgMThyZW0pKTtcbiAgICBnYXA6IDRyZW0gMXJlbTtcbiAgfVxuXG4gICY6Om5nLWRlZXAge1xuICAgIC51aS1ncmlkLWxpc3QtaXRlbSB7XG4gICAgICBAaW5jbHVkZSBkLWZsZXg7XG4gICAgICBhbGlnbi1pdGVtczogc3RyZXRjaDtcbiAgICAgIGZsZXgtZGlyZWN0aW9uOiBjb2x1bW47XG4gICAgICB0cmFuc2l0aW9uOiB0cmFuc2Zvcm0gMTUwbXMgY3ViaWMtYmV6aWVyKDAuNCwgMCwgMC4yLCAxKSAwcztcblxuICAgICAgJjphZnRlciB7XG4gICAgICAgIGNvbnRlbnQ6ICcnO1xuICAgICAgICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gICAgICAgIHotaW5kZXg6IC05OTtcbiAgICAgICAgdG9wOiAwcHg7XG4gICAgICAgIGxlZnQ6IDBweDtcbiAgICAgICAgd2lkdGg6IDEwMCU7XG4gICAgICAgIGhlaWdodDogMTAwJTtcbiAgICAgICAgdHJhbnNmb3JtOiBzY2FsZVkoMCk7XG4gICAgICAgIHRyYW5zZm9ybS1vcmlnaW46IGNlbnRlciB0b3A7XG4gICAgICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1wYXBlcik7XG4gICAgICAgIGJveC1zaGFkb3c6IHJnYigwIDAgMCAvIDIwJSkgMCAycHggMXB4IC0xcHgsXG4gICAgICAgICAgcmdiKDAgMCAwIC8gMTQlKSAwcHggMXB4IDFweCAwcHgsIHJnYigwIDAgMCAvIDEyJSkgMHB4IDFweCAzcHggMHB4O1xuICAgICAgfVxuXG4gICAgICAmOmhvdmVyIHtcbiAgICAgICAgdHJhbnNmb3JtOiBzY2FsZSgxLjAzKTtcblxuICAgICAgICAmOmFmdGVyIHtcbiAgICAgICAgICB0cmFuc2Zvcm06IHNjYWxlKDEpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICB9XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 4141:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/ui/pattern/movie-list/movie-list.component.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   MovieListComponent: () => (/* binding */ MovieListComponent)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_cdk_coerceObservable__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../shared/cdk/coerceObservable */ 4369);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _star_rating_star_rating_component__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../star-rating/star-rating.component */ 8574);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _shared_cdk_element_visibility_element_visibility_directive__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../../shared/cdk/element-visibility/element-visibility.directive */ 2625);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);
/* harmony import */ var _component_grid_list_grid_list_component__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../component/grid-list/grid-list.component */ 7866);
/* harmony import */ var _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! @rx-angular/template/if */ 1989);















const _c0 = function (a1) {
  return ["/detail/movie", a1];
};
function MovieListComponent_ui_grid_list_0_a_1_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "a", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](1, "img", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](2, "div", 6)(3, "h3", 7);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](5, "ui-star-rating", 8);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const movie_r4 = ctx.$implicit;
    const idx_r5 = ctx.index;
    const ctx_r3 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵpureFunction1"](11, _c0, movie_r4.id));
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵattribute"]("data-uf", "movie-" + idx_r5);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngSrc", movie_r4.imgSrc)("ngSrcset", movie_r4.imgSrcset)("sizes", movie_r4.imgSizes)("priority", idx_r5 < ctx_r3.numPriority())("width", movie_r4.imgWidth)("height", movie_r4.imgHeight)("title", movie_r4.title);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtextInterpolate1"](" ", movie_r4.title, " ");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rating", movie_r4.vote_average);
  }
}
function MovieListComponent_ui_grid_list_0_Template(rf, ctx) {
  if (rf & 1) {
    const _r7 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "ui-grid-list");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](1, MovieListComponent_ui_grid_list_0_a_1_Template, 6, 13, "a", 2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](2, "div", 3);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵlistener"]("elementVisibility", function MovieListComponent_ui_grid_list_0_Template_div_elementVisibility_2_listener($event) {
      _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵrestoreView"](_r7);
      const ctx_r6 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵresetView"](ctx_r6.ui.paginate($event));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const ctx_r0 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rxForOf", ctx_r0.movies$)("rxForTrackBy", ctx_r0.trackByMovieId);
  }
}
function MovieListComponent_ng_template_1_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "div", 9)(1, "span", 10);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](2, "No results");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](3, "fast-svg", 11);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
  }
}
class MovieListComponent {
  numPriority() {
    return this.state.get('numPriority');
  }
  set withImgPriority(p) {
    if (p) {
      this.state.set({
        numPriority: p
      });
    } else {
      this.state.set({
        numPriority: 0
      });
    }
  }
  set movies(movies$) {
    // eslint-disable-next-line @rx-angular/no-rxstate-subscriptions-outside-constructor
    this.state.connect('movies', (0,_shared_cdk_coerceObservable__WEBPACK_IMPORTED_MODULE_0__.coerceObservable)(movies$));
  }
  constructor(actions) {
    this.actions = actions;
    this.state = (0,_angular_core__WEBPACK_IMPORTED_MODULE_4__.inject)(_rx_angular_state__WEBPACK_IMPORTED_MODULE_5__.RxState);
    this.ui = this.actions.create();
    this.movies$ = this.state.select('movies');
    // if no movies are given we don't need to render nor listen for the infinite scroll trigger
    this.moviesListVisible$ = this.state.select((0,rxjs__WEBPACK_IMPORTED_MODULE_6__.map)(state => !!state.movies && state.movies.length > 0));
    // emit paginate event only if element is visible (true)
    this.paginate = this.ui.paginate$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.filter)(Boolean));
    this.state.set({
      numPriority: 2
    });
  }
  trackByMovieId(_, movie) {
    return movie.id;
  }
  static #_ = this.ɵfac = function MovieListComponent_Factory(t) {
    return new (t || MovieListComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_8__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵdefineComponent"]({
    type: MovieListComponent,
    selectors: [["ui-movie-list"]],
    inputs: {
      withImgPriority: "withImgPriority",
      movies: "movies"
    },
    outputs: {
      paginate: "paginate"
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵProvidersFeature"]([_rx_angular_state__WEBPACK_IMPORTED_MODULE_5__.RxState, _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_8__.RxActionFactory]), _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵStandaloneFeature"]],
    decls: 3,
    vars: 2,
    consts: [[4, "rxIf", "rxIfElse"], ["noData", ""], ["class", "ui-grid-list-item", 3, "routerLink", 4, "rxFor", "rxForOf", "rxForTrackBy"], [3, "elementVisibility"], [1, "ui-grid-list-item", 3, "routerLink"], ["alt", "poster movie", 1, "aspectRatio-2-3", "gradient", 3, "ngSrc", "ngSrcset", "sizes", "priority", "width", "height", "title"], [1, "movies-list--details"], [1, "movies-list--details-title"], [3, "rating"], [2, "display", "flex", "align-items", "center"], [2, "font-size", "1.5rem"], ["name", "sad"]],
    template: function MovieListComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](0, MovieListComponent_ui_grid_list_0_Template, 3, 2, "ui-grid-list", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](1, MovieListComponent_ng_template_1_Template, 4, 0, "ng-template", null, 1, _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplateRefExtractor"]);
      }
      if (rf & 2) {
        const _r1 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵreference"](2);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rxIf", ctx.moviesListVisible$)("rxIfElse", _r1);
      }
    },
    dependencies: [_angular_router__WEBPACK_IMPORTED_MODULE_9__.RouterLink, _star_rating_star_rating_component__WEBPACK_IMPORTED_MODULE_1__.StarRatingComponent, _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_10__.RxFor, _shared_cdk_element_visibility_element_visibility_directive__WEBPACK_IMPORTED_MODULE_2__.ElementVisibilityDirective, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_11__.FastSvgComponent, _component_grid_list_grid_list_component__WEBPACK_IMPORTED_MODULE_3__.GridListComponent, _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_12__.RxIf, _angular_common__WEBPACK_IMPORTED_MODULE_13__.NgOptimizedImage],
    styles: [".aspectRatio-2-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-2-3);\n}\n\n.aspectRatio-16-9[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-16-9);\n}\n\n.aspectRatio-4-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-4-3);\n}\n\n.fit-cover[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  object-fit: cover;\n  height: 100%;\n}\n\n.movies-list[_ngcontent-%COMP%] {\n  contain: content;\n}\n.movies-list--details[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  flex-direction: column;\n  justify-content: space-between;\n  padding: 1.5rem 3rem;\n}\n@media only screen and (max-width: 500px) {\n  .movies-list--details[_ngcontent-%COMP%] {\n    padding: 1.5rem;\n  }\n}\n.movies-list--details-title[_ngcontent-%COMP%] {\n  text-align: center;\n  margin-bottom: 1rem;\n  line-height: 1.4;\n}\n\n.ui-grid-list-item[_ngcontent-%COMP%] {\n  content-visibility: auto;\n  contain-intrinsic-size: 450px;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9hc3BlY3QtcmF0aW8vYXNwZWN0LXJhdGlvLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS9wYXR0ZXJuL21vdmllLWxpc3QvbW92aWUtbGlzdC5jb21wb25lbnQuc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL21peGlucy9fZmxleC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsMENBQUE7QUNDRjs7QURDQTtFQUNFLFdBQUE7RUFDQSxjQUFBO0VBQ0EsWUFBQTtFQUNBLDJDQUFBO0FDRUY7O0FEQUE7RUFDRSxXQUFBO0VBQ0EsY0FBQTtFQUNBLFlBQUE7RUFDQSwwQ0FBQTtBQ0dGOztBREFBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxpQkFBQTtFQUNBLFlBQUE7QUNHRjs7QUF2QkE7RUFDRSxnQkFBQTtBQTBCRjtBQXpCRTtFQ0dBLGFBQUE7RUFDQSxtQkFBQTtFREZFLHNCQUFBO0VBQ0EsOEJBQUE7RUFDQSxvQkFBQTtBQTRCSjtBQTNCSTtFQUxGO0lBTUksZUFBQTtFQThCSjtBQUNGO0FBNUJJO0VBQ0Usa0JBQUE7RUFDQSxtQkFBQTtFQUNBLGdCQUFBO0FBOEJOOztBQXpCQTtFQUNFLHdCQUFBO0VBQ0EsNkJBQUE7QUE0QkYiLCJzb3VyY2VzQ29udGVudCI6WyIuYXNwZWN0UmF0aW8tMi0zIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMi0zKTtcbn1cbi5hc3BlY3RSYXRpby0xNi05IHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMTYtOSk7XG59XG4uYXNwZWN0UmF0aW8tNC0zIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tNC0zKTtcbn1cblxuLmZpdC1jb3ZlciB7XG4gIHdpZHRoOiAxMDAlO1xuICBkaXNwbGF5OiBibG9jaztcbiAgb2JqZWN0LWZpdDogY292ZXI7XG4gIGhlaWdodDogMTAwJTtcbn1cbiIsIkBpbXBvcnQgJy4uLy4uL3Rva2VuL21peGlucy9mbGV4JztcbkBpbXBvcnQgJy4uLy4uL2NvbXBvbmVudC9hc3BlY3QtcmF0aW8vYXNwZWN0LXJhdGlvJztcblxuLm1vdmllcy1saXN0IHtcbiAgY29udGFpbjogY29udGVudDtcbiAgJi0tZGV0YWlscyB7XG4gICAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbiAgICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWJldHdlZW47XG4gICAgcGFkZGluZzogMS41cmVtIDNyZW07XG4gICAgQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA1MDBweCkge1xuICAgICAgcGFkZGluZzogMS41cmVtO1xuICAgIH1cblxuICAgICYtdGl0bGUge1xuICAgICAgdGV4dC1hbGlnbjogY2VudGVyO1xuICAgICAgbWFyZ2luLWJvdHRvbTogMXJlbTtcbiAgICAgIGxpbmUtaGVpZ2h0OiAxLjQ7XG4gICAgfVxuICB9XG59XG5cbi51aS1ncmlkLWxpc3QtaXRlbSB7XG4gIGNvbnRlbnQtdmlzaWJpbGl0eTogYXV0bztcbiAgY29udGFpbi1pbnRyaW5zaWMtc2l6ZTogNDUwcHg7XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ }),

/***/ 8574:
/*!*********************************************************************************!*\
  !*** ./projects/movies/src/app/ui/pattern/star-rating/star-rating.component.ts ***!
  \*********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   StarRatingComponent: () => (/* binding */ StarRatingComponent)
/* harmony export */ });
/* harmony import */ var _shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../shared/cdk/track-by */ 5221);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);



const _c0 = function (a0, a1) {
  return {
    "star-half": a0,
    "star-empty": a1
  };
};
function StarRatingComponent_span_3_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](0, "span", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](1, "\u2605");
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const fill_r2 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("ngClass", _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵpureFunction2"](1, _c0, fill_r2 === 0, fill_r2 === -1));
  }
}
function StarRatingComponent_div_4_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](0, "div", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const ctx_r1 = _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵnextContext"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtextInterpolate"](ctx_r1.rating);
  }
}
const range = 10;
const numStars = 5;
const starsArray = new Array(numStars).fill(1);
class StarRatingComponent {
  constructor() {
    this.range = range;
    this.numStars = numStars;
    this.stars = starsArray;
    this.showRating = false;
    this.tooltipText = `0 average rating`;
    this.trackByIndex = (0,_shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__.trackByIndex)();
    this._rating = 5;
  }
  set rating(rating) {
    this._rating = rating || 0;
    this.setToolTopText(this.rating);
    const scaledRating = this._rating / (this.range / this.numStars);
    const full = Math.floor(scaledRating);
    const half = scaledRating % 1 > 0.5 ? 1 : 0;
    const empty = this.numStars - full - half;
    this.stars = new Array(full).fill(1).concat(new Array(half).fill(0)).concat(new Array(empty).fill(-1));
  }
  get rating() {
    return this._rating;
  }
  setToolTopText(rating) {
    this.tooltipText = `${rating} average rating`;
  }
  static #_ = this.ɵfac = function StarRatingComponent_Factory(t) {
    return new (t || StarRatingComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineComponent"]({
    type: StarRatingComponent,
    selectors: [["ui-star-rating"]],
    inputs: {
      showRating: "showRating",
      rating: "rating"
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵStandaloneFeature"]],
    decls: 5,
    vars: 4,
    consts: [[1, "tooltip"], [1, "stars"], ["class", "star", 3, "ngClass", 4, "ngFor", "ngForOf", "ngForTrackBy"], ["class", "rating-value", 4, "ngIf"], [1, "star", 3, "ngClass"], [1, "rating-value"]],
    template: function StarRatingComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](0, "span", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtext"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementStart"](2, "div", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtemplate"](3, StarRatingComponent_span_3_Template, 2, 4, "span", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtemplate"](4, StarRatingComponent_div_4_Template, 2, 1, "div", 3);
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵtextInterpolate1"](" ", ctx.tooltipText, " ");
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](2);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("ngForOf", ctx.stars)("ngForTrackBy", ctx.trackByIndex);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵproperty"]("ngIf", ctx.showRating);
      }
    },
    dependencies: [_angular_common__WEBPACK_IMPORTED_MODULE_2__.NgFor, _angular_common__WEBPACK_IMPORTED_MODULE_2__.NgIf, _angular_common__WEBPACK_IMPORTED_MODULE_2__.NgClass],
    styles: ["@charset \"UTF-8\";\n[_nghost-%COMP%] {\n  position: relative;\n  display: flex;\n  align-items: center;\n  justify-content: center;\n}\n[_nghost-%COMP%]:hover   .tooltip[_ngcontent-%COMP%] {\n  visibility: visible;\n}\n\n.tooltip[_ngcontent-%COMP%] {\n  top: -28px;\n}\n\n.rating-value[_ngcontent-%COMP%] {\n  overflow: hidden;\n  color: var(--palette-warning-dark);\n  font-size: var(--text-sm);\n  padding: 5px 0 0 5px;\n}\n\n.stars[_ngcontent-%COMP%] {\n  overflow: hidden;\n  position: relative;\n}\n\n.star[_ngcontent-%COMP%] {\n  position: relative;\n  overflow: hidden;\n  cursor: pointer;\n  display: block;\n  float: left;\n  font-size: 24px;\n  color: var(--palette-warning-main);\n}\n\n.star-full[_ngcontent-%COMP%] {\n  color: var(--palette-warning-main);\n}\n\n.star-half[_ngcontent-%COMP%] {\n  color: var(--palette-text-secondary);\n}\n.star-half[_ngcontent-%COMP%]:before {\n  position: absolute;\n  overflow: hidden;\n  display: block;\n  z-index: 1;\n  top: 0;\n  left: 0;\n  width: 50%;\n  content: \"\u2605\";\n  color: var(--palette-warning-main);\n}\n\n.star-empty[_ngcontent-%COMP%] {\n  color: var(--palette-text-secondary);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3BhdHRlcm4vc3Rhci1yYXRpbmcvc3Rhci1yYXRpbmcuY29tcG9uZW50LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS90b2tlbi9taXhpbnMvX2ZsZXguc2NzcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQSxnQkFBZ0I7QUFFaEI7RUFDRSxrQkFBQTtFQ2VBLGFBQUE7RUFDQSxtQkFBQTtFQUNBLHVCQUFBO0FEZEY7QUFBRTtFQUNFLG1CQUFBO0FBRUo7O0FBRUE7RUFDRSxVQUFBO0FBQ0Y7O0FBRUE7RUFDRSxnQkFBQTtFQUNBLGtDQUFBO0VBQ0EseUJBQUE7RUFDQSxvQkFBQTtBQUNGOztBQUVBO0VBQ0UsZ0JBQUE7RUFDQSxrQkFBQTtBQUNGOztBQUVBO0VBQ0Usa0JBQUE7RUFDQSxnQkFBQTtFQUNBLGVBQUE7RUFDQSxjQUFBO0VBQ0EsV0FBQTtFQUNBLGVBQUE7RUFDQSxrQ0FBQTtBQUNGOztBQUVBO0VBQ0Usa0NBQUE7QUFDRjs7QUFFQTtFQUNFLG9DQUFBO0FBQ0Y7QUFDRTtFQUNFLGtCQUFBO0VBQ0EsZ0JBQUE7RUFDQSxjQUFBO0VBQ0EsVUFBQTtFQUNBLE1BQUE7RUFDQSxPQUFBO0VBQ0EsVUFBQTtFQUNBLFlBQUE7RUFDQSxrQ0FBQTtBQUNKOztBQUdBO0VBQ0Usb0NBQUE7QUFBRiIsInNvdXJjZXNDb250ZW50IjpbIkBpbXBvcnQgJy4uLy4uL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICBwb3NpdGlvbjogcmVsYXRpdmU7XG4gIEBpbmNsdWRlIGQtZmxleC12aDtcblxuICAmOmhvdmVyIC50b29sdGlwIHtcbiAgICB2aXNpYmlsaXR5OiB2aXNpYmxlO1xuICB9XG59XG5cbi50b29sdGlwIHtcbiAgdG9wOiAtMjhweDtcbn1cblxuLnJhdGluZy12YWx1ZSB7XG4gIG92ZXJmbG93OiBoaWRkZW47XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXdhcm5pbmctZGFyayk7XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1zbSk7XG4gIHBhZGRpbmc6IDVweCAwIDAgNXB4O1xufVxuXG4uc3RhcnMge1xuICBvdmVyZmxvdzogaGlkZGVuO1xuICBwb3NpdGlvbjogcmVsYXRpdmU7XG59XG5cbi5zdGFyIHtcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuICBvdmVyZmxvdzogaGlkZGVuO1xuICBjdXJzb3I6IHBvaW50ZXI7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBmbG9hdDogbGVmdDtcbiAgZm9udC1zaXplOiAyNHB4O1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS13YXJuaW5nLW1haW4pO1xufVxuXG4uc3Rhci1mdWxsIHtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtd2FybmluZy1tYWluKTtcbn1cblxuLnN0YXItaGFsZiB7XG4gIGNvbG9yOiB2YXIoLS1wYWxldHRlLXRleHQtc2Vjb25kYXJ5KTtcblxuICAmOmJlZm9yZSB7XG4gICAgcG9zaXRpb246IGFic29sdXRlO1xuICAgIG92ZXJmbG93OiBoaWRkZW47XG4gICAgZGlzcGxheTogYmxvY2s7XG4gICAgei1pbmRleDogMTtcbiAgICB0b3A6IDA7XG4gICAgbGVmdDogMDtcbiAgICB3aWR0aDogNTAlO1xuICAgIGNvbnRlbnQ6ICfDosKYwoUnO1xuICAgIGNvbG9yOiB2YXIoLS1wYWxldHRlLXdhcm5pbmctbWFpbik7XG4gIH1cbn1cblxuLnN0YXItZW1wdHkge1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS10ZXh0LXNlY29uZGFyeSk7XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */", ".tooltip[_ngcontent-%COMP%] {\n  position: absolute;\n  visibility: hidden;\n  width: max-content;\n  font-size: var(--text-sm);\n  background-color: var(--palette-primary-light);\n  color: var(--palette-primary-contrast-text);\n  text-align: center;\n  border-radius: var(--theme-borderRadius-sm);\n  padding: 1rem;\n  z-index: var(--theme-tooptip-zIndex);\n  transition: visibility var(--theme-anim-duration-short) var(--theme-anim-easing-easeOut);\n}\n\n.tooltip[_ngcontent-%COMP%]::after {\n  content: \"\";\n  position: absolute;\n  top: 100%;\n  left: 50%;\n  margin-left: -5px;\n  border-width: 5px;\n  border-style: solid;\n  border-color: var(--palette-primary-light) transparent transparent transparent;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC90b29sdGlwL190b29sdGlwLnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7RUFDRSxrQkFBQTtFQUVBLGtCQUFBO0VBQ0Esa0JBQUE7RUFDQSx5QkFBQTtFQUNBLDhDQUFBO0VBQ0EsMkNBQUE7RUFDQSxrQkFBQTtFQUNBLDJDQUFBO0VBQ0EsYUFBQTtFQUNBLG9DQUFBO0VBQ0Esd0ZBQUE7QUFBRjs7QUFJQTtFQUNFLFdBQUE7RUFDQSxrQkFBQTtFQUNBLFNBQUE7RUFDQSxTQUFBO0VBQ0EsaUJBQUE7RUFDQSxpQkFBQTtFQUNBLG1CQUFBO0VBQ0EsOEVBQUE7QUFERiIsInNvdXJjZXNDb250ZW50IjpbIi50b29sdGlwIHtcbiAgcG9zaXRpb246IGFic29sdXRlO1xuXG4gIHZpc2liaWxpdHk6IGhpZGRlbjtcbiAgd2lkdGg6IG1heC1jb250ZW50O1xuICBmb250LXNpemU6IHZhcigtLXRleHQtc20pO1xuICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbGlnaHQpO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWNvbnRyYXN0LXRleHQpO1xuICB0ZXh0LWFsaWduOiBjZW50ZXI7XG4gIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1zbSk7XG4gIHBhZGRpbmc6IDFyZW07XG4gIHotaW5kZXg6IHZhcigtLXRoZW1lLXRvb3B0aXAtekluZGV4KTtcbiAgdHJhbnNpdGlvbjogdmlzaWJpbGl0eSB2YXIoLS10aGVtZS1hbmltLWR1cmF0aW9uLXNob3J0KVxuICAgIHZhcigtLXRoZW1lLWFuaW0tZWFzaW5nLWVhc2VPdXQpO1xufVxuXG4udG9vbHRpcDo6YWZ0ZXIge1xuICBjb250ZW50OiAnJztcbiAgcG9zaXRpb246IGFic29sdXRlO1xuICB0b3A6IDEwMCU7XG4gIGxlZnQ6IDUwJTtcbiAgbWFyZ2luLWxlZnQ6IC01cHg7XG4gIGJvcmRlci13aWR0aDogNXB4O1xuICBib3JkZXItc3R5bGU6IHNvbGlkO1xuICBib3JkZXItY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1saWdodCkgdHJhbnNwYXJlbnQgdHJhbnNwYXJlbnQgdHJhbnNwYXJlbnQ7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ })

}]);
//# sourceMappingURL=default-projects_movies_src_app_ui_pattern_movie-list_movie-list_component_ts.js.map