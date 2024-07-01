"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_movie-list-page_movie-list-page_component_ts"],{

/***/ 6233:
/*!******************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/search.resource.ts ***!
  \******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   SearchResource: () => (/* binding */ SearchResource)
/* harmony export */ });
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common/http */ 4860);




const URL_SEARCH = [_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__.baseUrlApiV3, 'search', 'movie'].join('/');
class SearchResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_2__.HttpClient);
    this.getSearch = (query, params = {}) => this.http.get(URL_SEARCH, {
      params: {
        query,
        ...params
      }
    });
  }
  static #_ = this.ɵfac = function SearchResource_Factory(t) {
    return new (t || SearchResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineInjectable"]({
    token: SearchResource,
    factory: SearchResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 6112:
/*!**********************************************************************************!*\
  !*** ./projects/movies/src/app/pages/movie-list-page/movie-list-page.adapter.ts ***!
  \**********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   MovieListPageAdapter: () => (/* binding */ MovieListPageAdapter)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_16__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! rxjs */ 6290);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_15__ = __webpack_require__(/*! rxjs */ 8989);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_17__ = __webpack_require__(/*! rxjs */ 890);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_18__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var _state_discover_state__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../state/discover.state */ 3722);
/* harmony import */ var _state_movie_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../state/movie.state */ 6345);
/* harmony import */ var _shared_router_router_state__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../shared/router/router.state */ 8202);
/* harmony import */ var _shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../shared/cdk/infinite-scroll/infiniteScroll */ 8762);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _data_access_api_resources_discover_resource__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../data-access/api/resources/discover.resource */ 9456);
/* harmony import */ var _data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ../../data-access/api/resources/movie.resource */ 1405);
/* harmony import */ var _data_access_api_resources_search_resource__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ../../data-access/api/resources/search.resource */ 6233);
/* harmony import */ var _data_access_api_resources_genre_resource__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ../../data-access/api/resources/genre.resource */ 3009);
/* harmony import */ var _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ../../data-access/images/image-sizes */ 4082);
/* harmony import */ var _shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! ../../shared/cdk/image/image-tag.transform */ 9699);
















const emptyResult$ = rxjs__WEBPACK_IMPORTED_MODULE_10__.EMPTY;
function transformToMovieModel(_res) {
  return (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_9__.addImageTag)(_res, {
    pathProp: 'poster_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_8__.W154H205,
    sizes: '(min-width: 600px) 21vw, 15vw',
    srcset: '185w, 342w'
  });
}
class MovieListPageAdapter extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_11__.RxState {
  getInitialFetchByType({
    type,
    identifier
  }) {
    if (type === 'category') {
      return this.movieState.categoryMoviesByIdCtx(identifier).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(({
        loading,
        value
      }) => ({
        loading,
        ...value
      })));
    } else if (type === 'genre') {
      return this.discoverState.genreMoviesByIdSlice(identifier).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(({
        loading,
        value
      }) => ({
        loading,
        ...value
      })));
    } else if (type === 'search') {
      return this.searchResource.getSearch(identifier);
    }
    return emptyResult$;
  }
  constructor() {
    super();
    this.movieState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_state_movie_state__WEBPACK_IMPORTED_MODULE_1__.MovieState);
    this.discoverState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_state_discover_state__WEBPACK_IMPORTED_MODULE_0__.DiscoverState);
    this.routerState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_shared_router_router_state__WEBPACK_IMPORTED_MODULE_2__.RouterState);
    this.discoverResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_data_access_api_resources_discover_resource__WEBPACK_IMPORTED_MODULE_4__.DiscoverResource);
    this.movieResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_5__.MovieResource);
    this.searchResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_data_access_api_resources_search_resource__WEBPACK_IMPORTED_MODULE_6__.SearchResource);
    this.genreResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_13__.inject)(_data_access_api_resources_genre_resource__WEBPACK_IMPORTED_MODULE_7__.GenreResource);
    this.actions = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_14__.RxActionFactory().create();
    this.movies$ = this.select((0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(({
      results
    }) => results?.map(transformToMovieModel)));
    this.paginate = this.actions.paginate;
    this.routerFetchEffect = ({
      layout,
      type,
      identifier
    }) => {
      if (layout === 'list' && type === 'category') {
        this.movieState.fetchCategoryMovies(identifier);
      } else if (layout === 'list' && type === 'genre') {
        this.discoverState.fetchDiscoverGenreMovies(identifier);
      }
    };
    const routerParamsFromPaginationTrigger$ = this.actions.paginate$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_15__.withLatestFrom)(this.routerState.routerParams$), (0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(([, routerParams]) => routerParams));
    this.connect('genres', this.genreResource.getGenresDictionaryCached());
    this.connect(this.routerState.routerParams$.pipe((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_16__.selectSlice)(['identifier', 'type'])));
    this.connect(
    // paginated results as container state
    this.routerState.routerParams$.pipe(
    // we emit if a change in identifier takes place (search query, category name, genre id)
    (0,rxjs__WEBPACK_IMPORTED_MODULE_17__.distinctUntilKeyChanged)('identifier'),
    // we clear the current result on route change with switchMap and restart the initial scroll
    (0,rxjs__WEBPACK_IMPORTED_MODULE_18__.switchMap)(({
      type,
      identifier
    }) => (0,_shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_3__.infiniteScroll)(options => getFetchByType(type, this.movieResource, this.discoverResource, this.searchResource)(identifier, options), routerParamsFromPaginationTrigger$, this.getInitialFetchByType({
      type,
      identifier
    })))));
    this.hold(this.routerState.routerParams$, this.routerFetchEffect);
  }
  static #_ = this.ɵfac = function MovieListPageAdapter_Factory(t) {
    return new (t || MovieListPageAdapter)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_13__["ɵɵdefineInjectable"]({
    token: MovieListPageAdapter,
    factory: MovieListPageAdapter.ɵfac,
    providedIn: 'root'
  });
}

function getFetchByType(type, movieResource, discoverResource, searchResource) {
  if (type === 'category') {
    return movieResource.getMovieCategory;
  } else if (type === 'search') {
    return searchResource.getSearch;
  } else if (type === 'genre') {
    return (with_genres, options) => discoverResource.getDiscoverMovies({
      ...options,
      with_genres
    });
  }
  return () => emptyResult$;
}

/***/ }),

/***/ 9074:
/*!************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/movie-list-page/movie-list-page.component.ts ***!
  \************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _movie_list_page_adapter__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./movie-list-page.adapter */ 6112);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @rx-angular/template/if */ 1989);
/* harmony import */ var _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../ui/pattern/movie-list/movie-list.component */ 4141);








function MovieListPageComponent_header_1_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "header")(1, "h1", 3);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](3, "h2", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const headings_r2 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtextInterpolate"](headings_r2.main);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtextInterpolate"](headings_r2.sub);
  }
}
function MovieListPageComponent_div_3_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelement"](0, "div", 5);
  }
}
class MovieListPageComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_movie_list_page_adapter__WEBPACK_IMPORTED_MODULE_0__.MovieListPageAdapter);
    this.movies$ = this.adapter.movies$;
    this.loading$ = this.adapter.select('loading');
    this.headings$ = this.adapter.select((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_3__.selectSlice)(['identifier', 'type', 'genres']), (0,rxjs__WEBPACK_IMPORTED_MODULE_4__.map)(toHeading));
    this.adapter.set({
      loading: true
    });
  }
  paginate() {
    this.adapter.paginate();
  }
  static #_ = this.ɵfac = function MovieListPageComponent_Factory(t) {
    return new (t || MovieListPageComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineComponent"]({
    type: MovieListPageComponent,
    selectors: [["ct-movies-list"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵStandaloneFeature"]],
    decls: 4,
    vars: 3,
    consts: [[4, "rxLet"], [3, "movies", "paginate"], ["class", "loader", 4, "rxIf"], ["data-uf", "header-main"], ["data-uf", "header-sub"], [1, "loader"]],
    template: function MovieListPageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "article");
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](1, MovieListPageComponent_header_1_Template, 5, 2, "header", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](2, "ui-movie-list", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("paginate", function MovieListPageComponent_Template_ui_movie_list_paginate_2_listener() {
          return ctx.paginate();
        });
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](3, MovieListPageComponent_div_3_Template, 1, 0, "div", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxLet", ctx.headings$);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("movies", ctx.movies$);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxIf", ctx.loading$);
      }
    },
    dependencies: [_rx_angular_template_let__WEBPACK_IMPORTED_MODULE_5__.RxLet, _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_6__.RxIf, _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_1__.MovieListComponent],
    styles: [".loader[_ngcontent-%COMP%] {\n  display: grid;\n  place-items: center;\n  min-width: 150px;\n  min-height: 150px;\n  width: 100%;\n}\n.loader[_ngcontent-%COMP%]:after {\n  content: \" \";\n  width: 3rem;\n  height: 3rem;\n  border-radius: var(--theme-borderRadius-circle);\n  background-color: var(--palette-primary-dark);\n  box-shadow: -5rem 0 0 var(--palette-primary-main);\n  animation: _ngcontent-%COMP%_circle-classic 1s ease-in-out infinite alternate;\n}\n\n.loader.center-v[_ngcontent-%COMP%] {\n  position: absolute;\n  top: 50%;\n  left: 50%;\n  transform: translate(-50%, -50%);\n}\n\n@keyframes _ngcontent-%COMP%_circle-classic {\n  0% {\n    opacity: 0.1;\n    transform: rotate(0deg) scale(0.5);\n  }\n  100% {\n    opacity: 1;\n    transform: rotate(360deg) scale(1.2);\n  }\n}\n[_nghost-%COMP%] {\n  width: 100%;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9sb2FkZXIvbG9hZGVyLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9wYWdlcy9tb3ZpZS1saXN0LXBhZ2UvbW92aWUtbGlzdC1wYWdlLmNvbXBvbmVudC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBO0VBQ0UsYUFBQTtFQUNBLG1CQUFBO0VBQ0EsZ0JBQUE7RUFDQSxpQkFBQTtFQUNBLFdBQUE7QUNDRjtBRENFO0VBQ0UsWUFBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0EsK0NBQUE7RUFDQSw2Q0FBQTtFQUNBLGlEQUFBO0VBQ0EsMkRBQUE7QUNDSjs7QURHQTtFQUNFLGtCQUFBO0VBQ0EsUUFBQTtFQUNBLFNBQUE7RUFDQSxnQ0FBQTtBQ0FGOztBREdBO0VBQ0U7SUFDRSxZQUFBO0lBQ0Esa0NBQUE7RUNBRjtFREVBO0lBQ0UsVUFBQTtJQUNBLG9DQUFBO0VDQUY7QUFDRjtBQTlCQTtFQUNFLFdBQUE7QUFnQ0YiLCJzb3VyY2VzQ29udGVudCI6WyIubG9hZGVyIHtcbiAgZGlzcGxheTogZ3JpZDtcbiAgcGxhY2UtaXRlbXM6IGNlbnRlcjtcbiAgbWluLXdpZHRoOiAxNTBweDtcbiAgbWluLWhlaWdodDogMTUwcHg7XG4gIHdpZHRoOiAxMDAlO1xuXG4gICY6YWZ0ZXIge1xuICAgIGNvbnRlbnQ6ICcgJztcbiAgICB3aWR0aDogM3JlbTtcbiAgICBoZWlnaHQ6IDNyZW07XG4gICAgYm9yZGVyLXJhZGl1czogdmFyKC0tdGhlbWUtYm9yZGVyUmFkaXVzLWNpcmNsZSk7XG4gICAgYmFja2dyb3VuZC1jb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWRhcmspO1xuICAgIGJveC1zaGFkb3c6IC01cmVtIDAgMCB2YXIoLS1wYWxldHRlLXByaW1hcnktbWFpbik7XG4gICAgYW5pbWF0aW9uOiBjaXJjbGUtY2xhc3NpYyAxcyBlYXNlLWluLW91dCBpbmZpbml0ZSBhbHRlcm5hdGU7XG4gIH1cbn1cblxuLmxvYWRlci5jZW50ZXItdiB7XG4gIHBvc2l0aW9uOiBhYnNvbHV0ZTtcbiAgdG9wOiA1MCU7XG4gIGxlZnQ6IDUwJTtcbiAgdHJhbnNmb3JtOiB0cmFuc2xhdGUoLTUwJSwgLTUwJSk7XG59XG5cbkBrZXlmcmFtZXMgY2lyY2xlLWNsYXNzaWMge1xuICAwJSB7XG4gICAgb3BhY2l0eTogMC4xO1xuICAgIHRyYW5zZm9ybTogcm90YXRlKDBkZWcpIHNjYWxlKDAuNSk7XG4gIH1cbiAgMTAwJSB7XG4gICAgb3BhY2l0eTogMTtcbiAgICB0cmFuc2Zvcm06IHJvdGF0ZSgzNjBkZWcpIHNjYWxlKDEuMik7XG4gIH1cbn1cbiIsIkBpbXBvcnQgJy4uLy4uL3VpL2NvbXBvbmVudC9sb2FkZXIvbG9hZGVyJztcbkBpbXBvcnQgJy4uLy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICB3aWR0aDogMTAwJTtcbn1cbiJdLCJzb3VyY2VSb290IjoiIn0= */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (MovieListPageComponent);
function toHeading(routerParams) {
  const {
    identifier,
    type,
    genres
  } = routerParams;
  // default
  let main = identifier;
  const sub = type;
  // genre identifier needs to get mapped to a real title
  if (type === 'genre') {
    main = genres[parseInt(identifier)].name;
  }
  main = main.replace(/[-_]/, ' ');
  return {
    main,
    sub
  };
}

/***/ }),

/***/ 9699:
/*!*************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/image/image-tag.transform.ts ***!
  \*************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addImageTag: () => (/* binding */ addImageTag)
/* harmony export */ });
/* harmony import */ var _constants__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../constants */ 4765);

function addImageTag(_res, options) {
  let {
    fallback
  } = options;
  const {
    pathProp,
    dims,
    sizes,
    srcset
  } = options;
  fallback = fallback || _constants__WEBPACK_IMPORTED_MODULE_0__.POSTER_FALLBACK;
  const res = _res;
  if (sizes !== undefined) {
    res.imgSizes = sizes || '';
    res.imgSrcset = srcset || '';
  }
  res.imgSrc = res[pathProp] ? res[pathProp] + '' : fallback;
  res.imgWidth = dims.WIDTH;
  res.imgHeight = dims.HEIGHT;
  res.imgRatio = res.imgWidth / res.imgHeight;
  return res;
}

/***/ }),

/***/ 3722:
/*!*********************************************************!*\
  !*** ./projects/movies/src/app/state/discover.state.ts ***!
  \*********************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   DiscoverState: () => (/* binding */ DiscoverState)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../shared/cdk/optimized-fetch */ 3467);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../shared/cdk/loading/withLoadingEmissions */ 9135);
/* harmony import */ var _data_access_api_resources_discover_resource__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../data-access/api/resources/discover.resource */ 9456);
/* harmony import */ var _shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../shared/cdk/get */ 2303);










class DiscoverState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_4__.RxState {
  constructor() {
    super();
    this.actionsF = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_5__.RxActionFactory();
    this.discoverResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_data_access_api_resources_discover_resource__WEBPACK_IMPORTED_MODULE_2__.DiscoverResource);
    this.actions = this.actionsF.create({
      fetchDiscoverGenreMovies: String,
      fetchDiscoverCastMovies: String
    });
    this.fetchDiscoverGenreMovies = this.actions.fetchDiscoverGenreMovies;
    this.genreMoviesByIdSlice = id => this.select((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(({
      genreMovies: {
        value,
        loading
      }
    }) => ({
      loading,
      value: (0,_shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__.pluck)(value, id)
    })));
    (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_6__.DestroyRef).onDestroy(() => this.actionsF.destroy());
    this.connect('genreMovies', this.actions.fetchDiscoverGenreMovies$.pipe((0,_shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__.optimizedFetch)(genre => genre, with_genres => this.discoverResource.getDiscoverMovies({
      with_genres,
      page: 1
    }).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(resp => ({
      value: {
        [with_genres]: resp
      }
    })), (0,_shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__.withLoadingEmission)()))), (oldState, newPartial) => {
      const resultState = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(oldState?.genreMovies, newPartial);
      resultState.value = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(oldState?.genreMovies?.value, resultState.value);
      return resultState;
    });
    this.connect('personMovies', this.actions.fetchDiscoverCastMovies$.pipe((0,_shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__.optimizedFetch)(person => person, with_cast => this.discoverResource.getDiscoverMovies({
      with_cast,
      page: 1
    }).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(resp => ({
      value: {
        [with_cast]: resp
      }
    })), (0,_shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__.withLoadingEmission)()))), (oldState, newPartial) => {
      const resultState = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(oldState?.personMovies, newPartial);
      resultState.value = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(oldState?.personMovies?.value, resultState.value);
      return resultState;
    });
  }
  initialize(category) {
    this.fetchDiscoverGenreMovies(category);
  }
  static #_ = this.ɵfac = function DiscoverState_Factory(t) {
    return new (t || DiscoverState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_6__["ɵɵdefineInjectable"]({
    token: DiscoverState,
    factory: DiscoverState.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 890:
/*!**********************************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/operators/distinctUntilKeyChanged.js ***!
  \**********************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   distinctUntilKeyChanged: () => (/* binding */ distinctUntilKeyChanged)
/* harmony export */ });
/* harmony import */ var _distinctUntilChanged__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./distinctUntilChanged */ 3317);

function distinctUntilKeyChanged(key, compare) {
  return (0,_distinctUntilChanged__WEBPACK_IMPORTED_MODULE_0__.distinctUntilChanged)((x, y) => compare ? compare(x[key], y[key]) : x[key] === y[key]);
}

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_movie-list-page_movie-list-page_component_ts.js.map