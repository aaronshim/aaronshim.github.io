"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_movie-detail-page_movie-detail-page_component_ts"],{

/***/ 5021:
/*!**************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/movie-detail-page/movie-detail-page.adapter.ts ***!
  \**************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   MovieDetailAdapter: () => (/* binding */ MovieDetailAdapter),
/* harmony export */   transformToCastList: () => (/* binding */ transformToCastList),
/* harmony export */   transformToMovieDetail: () => (/* binding */ transformToMovieDetail),
/* harmony export */   transformToMovieModel: () => (/* binding */ transformToMovieModel)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _shared_router_router_state__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../shared/router/router.state */ 8202);
/* harmony import */ var _state_movie_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../state/movie.state */ 6345);
/* harmony import */ var _shared_router_get_identifier_of_type_and_layout_util__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../shared/router/get-identifier-of-type-and-layout.util */ 5176);
/* harmony import */ var _data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../data-access/api/resources/movie.resource */ 1405);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../shared/cdk/infinite-scroll/infiniteScroll */ 8762);
/* harmony import */ var _shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ../../shared/cdk/loading/withLoadingEmissions */ 9135);
/* harmony import */ var _shared_cdk_video_video_tag_transform__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ../../shared/cdk/video/video-tag.transform */ 9705);
/* harmony import */ var _shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! ../../shared/cdk/image/image-tag.transform */ 9699);
/* harmony import */ var _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! ../../data-access/images/image-sizes */ 4082);
/* harmony import */ var _shared_cdk_link_a_tag_transform__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! ../../shared/cdk/link/a-tag.transform */ 746);
/* harmony import */ var _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @rx-angular/state/effects */ 7448);















class MovieDetailAdapter extends _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_10__.RxEffects {
  constructor(errorHandler) {
    super(errorHandler);
    this.movieState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_11__.inject)(_state_movie_state__WEBPACK_IMPORTED_MODULE_1__.MovieState);
    this.routerState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_11__.inject)(_shared_router_router_state__WEBPACK_IMPORTED_MODULE_0__.RouterState);
    this.movieResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_11__.inject)(_data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_3__.MovieResource);
    this.actions = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_12__.RxActionFactory().create();
    this.paginateRecommendations = this.actions.paginateRecommendations;
    this.routerMovieId$ = this.routerState.select((0,_shared_router_get_identifier_of_type_and_layout_util__WEBPACK_IMPORTED_MODULE_2__.getIdentifierOfTypeAndLayoutUtil)('movie', 'detail'));
    this.routedMovieCtx$ = this.routerMovieId$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_13__.switchMap)(this.movieState.movieByIdCtx), (0,rxjs__WEBPACK_IMPORTED_MODULE_14__.map)(ctx => {
      ctx.value && (ctx.value = transformToMovieDetail(ctx.value));
      return ctx;
    }));
    this.movieCastById$ = this.routerMovieId$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_13__.switchMap)(id => this.movieResource.getCredits(id).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_14__.map)(({
      cast
    }) => ({
      value: cast.map(transformToCastList)
    })))), (0,_shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_5__.withLoadingEmission)());
    this.infiniteScrollRecommendations$ = this.routerMovieId$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_13__.switchMap)(id => (0,_shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_4__.infiniteScroll)(incrementedParams => this.movieResource.getMoviesRecommendations(id, incrementedParams), this.actions.paginateRecommendations$, this.movieResource.getMoviesRecommendations(id, {
      page: 1
    }))), (0,rxjs__WEBPACK_IMPORTED_MODULE_14__.map)(v => ({
      ...v,
      results: v?.results?.map(transformToMovieModel)
    })));
    this.register(this.routerMovieId$, this.movieState.fetchMovie);
  }
  static #_ = this.ɵfac = function MovieDetailAdapter_Factory(t) {
    return new (t || MovieDetailAdapter)(_angular_core__WEBPACK_IMPORTED_MODULE_11__["ɵɵinject"](_angular_core__WEBPACK_IMPORTED_MODULE_11__.ErrorHandler));
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_11__["ɵɵdefineInjectable"]({
    token: MovieDetailAdapter,
    factory: MovieDetailAdapter.ɵfac,
    providedIn: 'root'
  });
}

function transformToMovieDetail(_res) {
  const res = _res;
  let language = false;
  if (Array.isArray(res?.spoken_languages) && res?.spoken_languages.length !== 0) {
    language = res.spoken_languages[0].english_name;
  }
  res.languages_runtime_release = `${language + ' / ' || 0} ${res.runtime} MIN. / ${new Date(res.release_date).getFullYear()}`;
  (0,_shared_cdk_video_video_tag_transform__WEBPACK_IMPORTED_MODULE_6__.addVideoTag)(res, {
    pathPropFn: r => r?.videos?.results && r?.videos?.results[0]?.key + '' || ''
  });
  (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_7__.addImageTag)(res, {
    pathProp: 'poster_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_8__.W300H450,
    sizes: `(min-width: 901px) 15vw, 42vw`,
    srcset: '154w, 185w, 342w, 500w, 780w'
  });
  (0,_shared_cdk_link_a_tag_transform__WEBPACK_IMPORTED_MODULE_9__.addLinkTag)(res, 'imdb_id', {});
  return res;
}
function transformToCastList(_res) {
  const res = _res;
  (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_7__.addImageTag)(res, {
    pathProp: 'profile_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_8__.W44H66,
    sizes: `44px`,
    srcset: '154w'
  });
  return res;
}
function transformToMovieModel(_res) {
  return (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_7__.addImageTag)(_res, {
    pathProp: 'poster_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_8__.W154H205,
    sizes: '(min-width: 600px) 21vw, 15vw',
    srcset: '185w, 342w'
  });
}

/***/ }),

/***/ 4909:
/*!****************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/movie-detail-page/movie-detail-page.component.ts ***!
  \****************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! rxjs */ 9690);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! rxjs */ 3738);
/* harmony import */ var _movie_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./movie-detail-page.adapter */ 5021);
/* harmony import */ var _rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @rx-angular/state/effects */ 7448);
/* harmony import */ var _ui_component_detail_grid_detail_grid_component__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../ui/component/detail-grid/detail-grid.component */ 3251);
/* harmony import */ var _ui_pattern_star_rating_star_rating_component__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../ui/pattern/star-rating/star-rating.component */ 8574);
/* harmony import */ var _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../ui/pattern/movie-list/movie-list.component */ 4141);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_17__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _shared_cdk_bypass_src_directive__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../shared/cdk/bypass-src.directive */ 2232);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_15__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_18__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);
/* harmony import */ var _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_16__ = __webpack_require__(/*! @rx-angular/template/if */ 1989);
/* harmony import */ var _angular_router__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! @angular/router */ 7947);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);

















const _c0 = ["trailerDialog"];
const _c1 = ["castListWrapper"];
const _c2 = function (a1) {
  return ["/list/genre/", a1];
};
function MovieDetailPageComponent_ng_container_1_a_19_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "a", 29);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](1, "fast-svg", 30);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const genre_r11 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵpureFunction1"](2, _c2, genre_r11.id));
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtextInterpolate1"](" ", genre_r11.name, " ");
  }
}
const _c3 = function (a1) {
  return ["/detail/person/", a1];
};
function MovieDetailPageComponent_ng_container_1_div_31_a_2_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "a", 34);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("click", function MovieDetailPageComponent_ng_container_1_div_31_a_2_Template_a_click_0_listener($event) {
      return $event.preventDefault();
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](1, "picture");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](2, "img", 35);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const c_r16 = ctx.$implicit;
    const idx_r17 = ctx.index;
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("routerLink", _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵpureFunction1"](10, _c3, c_r16.id));
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵattribute"]("data-uf", "cast-" + idx_r17)("aria-label", c_r16.name);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngSrc", c_r16.imgSrc)("ngSrcset", c_r16.imgSrcset)("sizes", c_r16.imgSizes)("width", c_r16.imgWidth)("height", c_r16.imgHeight)("alt", c_r16.name)("title", c_r16.name);
  }
}
function MovieDetailPageComponent_ng_container_1_div_31_div_3_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](0, "div", 36);
  }
}
function MovieDetailPageComponent_ng_container_1_div_31_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "div", 31, 32);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](2, MovieDetailPageComponent_ng_container_1_div_31_a_2_Template, 3, 12, "a", 33);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](3, MovieDetailPageComponent_ng_container_1_div_31_div_3_Template, 1, 0, "div", 28);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const ctx_r12 = ctx.$implicit;
    const ctx_r4 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("rxForOf", ctx_r12.value)("rxForTrackBy", ctx_r4.trackByCast);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngIf", ctx_r12.loading);
  }
}
function MovieDetailPageComponent_ng_container_1_a_35_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "a", 37);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](1, " Website ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](2, "fast-svg", 38);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const movie_r2 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]().$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("href", movie_r2.homepage, _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵsanitizeUrl"]);
  }
}
function MovieDetailPageComponent_ng_container_1_a_36_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "a", 39);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](1, " IMDB ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](2, "fast-svg", 40);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const movie_r2 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]().$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("target", movie_r2.target)("href", movie_r2.href, _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵsanitizeUrl"]);
  }
}
function MovieDetailPageComponent_ng_container_1_button_37_Template(rf, ctx) {
  if (rf & 1) {
    const _r22 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "button", 41);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("mouseover", function MovieDetailPageComponent_ng_container_1_button_37_Template_button_mouseover_0_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r22);
      const ctx_r21 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r21.ui.iframe("load"));
    })("focus", function MovieDetailPageComponent_ng_container_1_button_37_Template_button_focus_0_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r22);
      const ctx_r23 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r23.ui.iframe("load"));
    })("click", function MovieDetailPageComponent_ng_container_1_button_37_Template_button_click_0_listener($event) {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r22);
      const ctx_r24 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"](2);
      $event.preventDefault();
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r24.ui({
        dialog: "show",
        iframe: "load"
      }));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](1, " Trailer ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](2, "fast-svg", 42);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
  }
}
function MovieDetailPageComponent_ng_container_1_iframe_42_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](0, "iframe", 43);
  }
  if (rf & 2) {
    const movie_r2 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]().$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("bypassSrc", movie_r2.videoUrl);
  }
}
function MovieDetailPageComponent_ng_container_1_div_46_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](0, "div", 36);
  }
}
function MovieDetailPageComponent_ng_container_1_Template(rf, ctx) {
  if (rf & 1) {
    const _r27 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](1, "ui-detail-grid")(2, "div", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](3, "img", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](4, "div", 6)(5, "header")(6, "h1", 7);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](7);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](8, "h2", 8);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](9);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](10, "section", 9);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](11, "ui-star-rating", 10);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](12, "div", 11)(13, "strong");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](14);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()()();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](15, "section")(16, "h3");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](17, "The Genres");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](18, "div", 12);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](19, MovieDetailPageComponent_ng_container_1_a_19_Template, 3, 4, "a", 13);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](20, "section")(21, "h3");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](22, "The Synopsis");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](23, "p");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](24);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](25, "section")(26, "h3");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](27, "The Cast");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](28, "div", 14)(29, "button", 15);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("mousedown", function MovieDetailPageComponent_ng_container_1_Template_button_mousedown_29_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r27);
      const ctx_r26 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r26.move(70));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](30, " \u00AB ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](31, MovieDetailPageComponent_ng_container_1_div_31_Template, 4, 3, "div", 16);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](32, "button", 17);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("mousedown", function MovieDetailPageComponent_ng_container_1_Template_button_mousedown_32_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r27);
      const ctx_r28 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r28.move(-70));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](33, " \u00BB ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()()();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](34, "section", 18);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](35, MovieDetailPageComponent_ng_container_1_a_35_Template, 3, 1, "a", 19);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](36, MovieDetailPageComponent_ng_container_1_a_36_Template, 3, 2, "a", 20);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](37, MovieDetailPageComponent_ng_container_1_button_37_Template, 3, 0, "button", 21);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](38, "dialog", 22, 23)(40, "button", 24);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("click", function MovieDetailPageComponent_ng_container_1_Template_button_click_40_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r27);
      const ctx_r29 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r29.ui.dialog("close"));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](41, " \u00A0 ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](42, MovieDetailPageComponent_ng_container_1_iframe_42_Template, 1, 1, "iframe", 25);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](43, "button", 26);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("click", function MovieDetailPageComponent_ng_container_1_Template_button_click_43_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r27);
      const ctx_r30 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r30.back());
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](44, "fast-svg", 27);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](45, "\u00A0Back ");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()()()();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](46, MovieDetailPageComponent_ng_container_1_div_46_Template, 1, 0, "div", 28);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const movie_r2 = ctx.$implicit;
    const ctx_r0 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵstyleProp"]("opacity", !!movie_r2 ? 1 : 0);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngSrc", movie_r2.imgSrc)("ngSrcset", movie_r2.imgSrcset)("sizes", movie_r2.imgSizes)("width", movie_r2.imgWidth)("height", movie_r2.imgHeight)("title", movie_r2.title);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtextInterpolate"](movie_r2.title);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtextInterpolate"](movie_r2.tagline);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("rating", movie_r2.vote_average)("showRating", true);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtextInterpolate"](movie_r2.languages_runtime_release);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](5);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngForOf", movie_r2.genres)("ngForTrackBy", ctx_r0.trackByGenre);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](5);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtextInterpolate"](movie_r2.overview || "There is no synopsis available...");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](7);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("rxLet", ctx_r0.castList$)("rxLetStrategy", "immediate");
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngIf", movie_r2.homepage);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngIf", movie_r2.imdb_id);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngIf", movie_r2.videoUrl);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](5);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("rxIf", ctx_r0.loadIframe$);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngIf", !movie_r2);
  }
}
function MovieDetailPageComponent_ng_container_8_div_2_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelement"](0, "div", 36);
  }
}
function MovieDetailPageComponent_ng_container_8_Template(rf, ctx) {
  if (rf & 1) {
    const _r34 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](1, "ui-movie-list", 44);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵlistener"]("paginate", function MovieDetailPageComponent_ng_container_8_Template_ui_movie_list_paginate_1_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵrestoreView"](_r34);
      const ctx_r33 = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵresetView"](ctx_r33.paginateRecommendations());
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](2, MovieDetailPageComponent_ng_container_8_div_2_Template, 1, 0, "div", 28);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const ctx_r31 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("movies", ctx_r31.results)("withImgPriority", 0);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("ngIf", ctx_r31.loading);
  }
}
class MovieDetailPageComponent {
  constructor(actionsF) {
    this.actionsF = actionsF;
    this.location = (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_6__.Location);
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_movie_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__.MovieDetailAdapter);
    this.effects = (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_7__.RxEffects);
    this.ui = this.actionsF.create();
    this.movieCtx$ = this.adapter.routedMovieCtx$;
    this.loadIframe$ = this.ui.iframe$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_8__.mergeWith)(this.movieCtx$.pipe(
    // select changes of video nested property
    (0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_9__.selectSlice)(['value'], {
      value: ({
        video
      }) => video
    }))), (0,rxjs__WEBPACK_IMPORTED_MODULE_10__.map)(e => e === 'load'));
    this.movie$ = this.movieCtx$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_10__.map)(ctx => ctx?.value || null), (0,rxjs__WEBPACK_IMPORTED_MODULE_11__.filter)(movie => !!movie));
    this.castList$ = this.adapter.movieCastById$;
    this.castListLoading$ = this.adapter.movieCastById$.pipe((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_9__.select)('loading'));
    this.infiniteScrollRecommendations$ = this.adapter.infiniteScrollRecommendations$;
    this.trailerDialog = undefined;
    this.castListWrapper = undefined;
    this.trackByGenre = (_, genre) => genre.name;
    this.trackByCast = (_, cast) => cast.cast_id;
    this.effects.register(this.ui.dialog$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_10__.map)(v => v === 'show'), (0,rxjs__WEBPACK_IMPORTED_MODULE_12__.tap)(console.log)), openDialog => openDialog ? this.trailerDialog?.nativeElement?.showModal() : this.trailerDialog?.nativeElement.close());
  }
  move(increment) {
    if (this.castListWrapper) {
      // eslint-disable-next-line @rx-angular/prefer-no-layout-sensitive-apis
      const scrollLeft = this.castListWrapper.nativeElement.scrollLeft;
      const newScrollLetf = scrollLeft - increment;
      // eslint-disable-next-line @rx-angular/prefer-no-layout-sensitive-apis
      this.castListWrapper.nativeElement.scrollLeft = newScrollLetf > 0 ? Math.max(0, newScrollLetf) : Math.min(newScrollLetf, this.castListWrapper.nativeElement.children.length * increment);
    }
  }
  back() {
    this.location.back();
  }
  paginateRecommendations() {
    this.adapter.paginateRecommendations();
  }
  static #_ = this.ɵfac = function MovieDetailPageComponent_Factory(t) {
    return new (t || MovieDetailPageComponent)(_angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵdirectiveInject"](_rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_13__.RxActionFactory));
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵdefineComponent"]({
    type: MovieDetailPageComponent,
    selectors: [["ct-movie"]],
    viewQuery: function MovieDetailPageComponent_Query(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵviewQuery"](_c0, 5);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵviewQuery"](_c1, 5);
      }
      if (rf & 2) {
        let _t;
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵqueryRefresh"](_t = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵloadQuery"]()) && (ctx.trailerDialog = _t.first);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵqueryRefresh"](_t = _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵloadQuery"]()) && (ctx.castListWrapper = _t.first);
      }
    },
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵProvidersFeature"]([_rx_angular_state_effects__WEBPACK_IMPORTED_MODULE_7__.RxEffects]), _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵStandaloneFeature"]],
    decls: 9,
    vars: 3,
    consts: [[1, "movie-detail-wrapper"], [4, "rxLet"], [1, "recommendations"], [4, "rxLet", "rxLetStrategy"], ["detailGridMedia", ""], ["priority", "high", "alt", "poster movie", "data-uf", "hero-img", 1, "aspectRatio-2-3", "fit-cover", 3, "ngSrc", "ngSrcset", "sizes", "width", "height", "title"], ["detailGridDescription", ""], ["data-uf", "header-main"], ["data-uf", "header-sub"], [1, "movie-detail--basic-infos"], [3, "rating", "showRating"], [1, "movie-detail--languages-runtime-release"], [1, "movie-detail--genres"], ["class", "movie-detail--genres-link", 3, "routerLink", 4, "ngFor", "ngForOf", "ngForTrackBy"], [1, "movie-detail--cast-list"], ["aria-label", "Scroll cast left", "type", "button", "role", "button", 1, "cast-list--btn", 3, "mousedown"], ["class", "cast-list", 4, "rxLet", "rxLetStrategy"], ["aria-label", "Scroll cast right", "type", "button", "role", "button", 1, "cast-list--btn", 3, "mousedown"], [1, "movie-detail--ad-section-links"], ["class", "btn", "target", "_blank", "rel", "noopener noreferrer", 3, "href", 4, "ngIf"], ["class", "btn", "rel", "noopener noreferrer", 3, "target", "href", 4, "ngIf"], ["type", "button", "class", "btn", 3, "mouseover", "focus", "click", 4, "ngIf"], ["id", "trailer-dialog", 1, "video"], ["trailerDialog", ""], ["aria-controls", "trailer-dialog", 1, "close", "functionality-only-button", 3, "click"], ["loading", "lazy", "width", "460", "height", "230", 3, "bypassSrc", 4, "rxIf"], ["aria-label", "Back", "data-uf", "back", 1, "btn", "primary-button", 3, "click"], ["name", "back", "size", "1em", 1, "btn__icon"], ["class", "loader", 4, "ngIf"], [1, "movie-detail--genres-link", 3, "routerLink"], ["name", "genre", 2, "margin-right", "0.5rem"], [1, "cast-list"], ["castListWrapper", ""], ["class", "movie-detail--cast-actor", 3, "routerLink", "click", 4, "rxFor", "rxForOf", "rxForTrackBy"], [1, "movie-detail--cast-actor", 3, "routerLink", "click"], [3, "ngSrc", "ngSrcset", "sizes", "width", "height", "alt", "title"], [1, "loader"], ["target", "_blank", "rel", "noopener noreferrer", 1, "btn", 3, "href"], ["name", "website", 1, "btn__icon"], ["rel", "noopener noreferrer", 1, "btn", 3, "target", "href"], ["name", "imdb", 1, "btn__icon"], ["type", "button", 1, "btn", 3, "mouseover", "focus", "click"], ["name", "play", 1, "btn__icon"], ["loading", "lazy", "width", "460", "height", "230", 3, "bypassSrc"], [3, "movies", "withImgPriority", "paginate"]],
    template: function MovieDetailPageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](0, "article", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](1, MovieDetailPageComponent_ng_container_1_Template, 47, 23, "ng-container", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](2, "article", 2)(3, "header")(4, "h1");
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](5, "Recommended");
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementStart"](6, "h2");
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtext"](7, "Movies");
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]()();
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵtemplate"](8, MovieDetailPageComponent_ng_container_8_Template, 3, 3, "ng-container", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("rxLet", ctx.movie$);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵadvance"](7);
        _angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵproperty"]("rxLet", ctx.infiniteScrollRecommendations$)("rxLetStrategy", "immediate");
      }
    },
    dependencies: [_angular_common__WEBPACK_IMPORTED_MODULE_6__.NgIf, _angular_common__WEBPACK_IMPORTED_MODULE_6__.NgFor, _angular_router__WEBPACK_IMPORTED_MODULE_14__.RouterLink, _angular_common__WEBPACK_IMPORTED_MODULE_6__.NgOptimizedImage, _ui_component_detail_grid_detail_grid_component__WEBPACK_IMPORTED_MODULE_1__.DetailGridComponent, _ui_pattern_star_rating_star_rating_component__WEBPACK_IMPORTED_MODULE_2__.StarRatingComponent, _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_3__.MovieListComponent, _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_15__.RxFor, _rx_angular_template_if__WEBPACK_IMPORTED_MODULE_16__.RxIf, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_17__.RxLet, _shared_cdk_bypass_src_directive__WEBPACK_IMPORTED_MODULE_4__.BypassSrcDirective, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_18__.FastSvgComponent],
    styles: [".aspectRatio-2-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-2-3);\n}\n\n.aspectRatio-16-9[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-16-9);\n}\n\n.aspectRatio-4-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-4-3);\n}\n\n.fit-cover[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  object-fit: cover;\n  height: 100%;\n}\n\n.loader[_ngcontent-%COMP%] {\n  display: grid;\n  place-items: center;\n  min-width: 150px;\n  min-height: 150px;\n  width: 100%;\n}\n.loader[_ngcontent-%COMP%]:after {\n  content: \" \";\n  width: 3rem;\n  height: 3rem;\n  border-radius: var(--theme-borderRadius-circle);\n  background-color: var(--palette-primary-dark);\n  box-shadow: -5rem 0 0 var(--palette-primary-main);\n  animation: _ngcontent-%COMP%_circle-classic 1s ease-in-out infinite alternate;\n}\n\n.loader.center-v[_ngcontent-%COMP%] {\n  position: absolute;\n  top: 50%;\n  left: 50%;\n  transform: translate(-50%, -50%);\n}\n\n@keyframes _ngcontent-%COMP%_circle-classic {\n  0% {\n    opacity: 0.1;\n    transform: rotate(0deg) scale(0.5);\n  }\n  100% {\n    opacity: 1;\n    transform: rotate(360deg) scale(1.2);\n  }\n}\n.btn[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  display: inline-flex;\n  outline: none;\n  padding: 6px 16px;\n  min-width: 96px;\n  min-height: 48px;\n  font-weight: normal;\n  font-size: var(--text-md);\n  color: var(--palette-primary-dark);\n  border: 1px solid rgba(var(--palette-primary-main-rgb), 0.5);\n  border-radius: var(--theme-borderRadius-m);\n  box-shadow: none;\n  background-color: transparent;\n  cursor: pointer;\n}\n\n.btn__icon[_ngcontent-%COMP%] {\n  width: 24px;\n  height: 24px;\n  margin-left: 8px;\n}\n\n.primary-button[_ngcontent-%COMP%] {\n  background: var(--palette-primary-main);\n  color: var(--palette-primary-contrast-text);\n}\n.primary-button[_ngcontent-%COMP%]:hover {\n  background-color: var(--palette-primary-light);\n}\n\n.functionality-only-button[_ngcontent-%COMP%] {\n  background: none;\n  border: none;\n  display: block;\n  text-align: left;\n  height: inherit;\n}\n\ndialog[_ngcontent-%COMP%]::backdrop {\n  background: rgba(0, 0, 0, 0.5);\n  transition: opacity var(--theme-anim-duration-leavingScreen) var(--theme-anim-easing-easeInOut) 0ms;\n}\ndialog[_ngcontent-%COMP%]   .close[_ngcontent-%COMP%] {\n  position: absolute;\n  width: 30px;\n  height: 30px;\n  top: -30px;\n  right: -30px;\n}\ndialog[_ngcontent-%COMP%]   .close[_ngcontent-%COMP%]::after, dialog[_ngcontent-%COMP%]   .close[_ngcontent-%COMP%]::before {\n  content: \"\";\n  position: absolute;\n  height: 2px;\n  width: 100%;\n  top: 50%;\n  left: 0px;\n  background: rgb(255, 255, 255);\n  transform: rotate(45deg);\n  border-radius: var(--theme-borderRadius-m);\n  margin-top: -6px;\n}\ndialog[_ngcontent-%COMP%]   .close[_ngcontent-%COMP%]::after {\n  transform: rotate(-45deg);\n}\ndialog.video[_ngcontent-%COMP%] {\n  aspect-ratio: var(--theme-aspectRatio-16-9);\n  width: 90%;\n}\ndialog[_ngcontent-%COMP%]   iframe[_ngcontent-%COMP%], dialog[_ngcontent-%COMP%]   video[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  object-fit: cover;\n  height: 100%;\n}\n\n[_nghost-%COMP%] {\n  width: 100%;\n  display: block;\n}\n\n.loader[_ngcontent-%COMP%] {\n  position: absolute;\n  z-index: 200;\n  top: 250px;\n}\n\n.movie-detail-wrapper[_ngcontent-%COMP%] {\n  min-height: 500px;\n}\n\n.movie-detail[_ngcontent-%COMP%] {\n  contain: content;\n}\n@media only screen and (max-width: 1500px) {\n  .movie-detail--grid-item[_ngcontent-%COMP%] {\n    padding: 3rem;\n  }\n}\n.movie-detail--genres[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  flex-wrap: wrap;\n}\n.movie-detail--genres-link[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  padding: 0.5rem 0;\n  font-weight: bold;\n  text-transform: uppercase;\n}\n.movie-detail--genres-link[_ngcontent-%COMP%]:not(:last-child) {\n  margin-right: 2rem;\n}\n.movie-detail--ad-section-links[_ngcontent-%COMP%]   .section--content[_ngcontent-%COMP%] {\n  display: flex;\n  margin-right: auto;\n}\n.movie-detail--ad-section-links[_ngcontent-%COMP%]   .btn[_ngcontent-%COMP%] {\n  margin-right: 2rem;\n}\n@media only screen and (max-width: 1300px) {\n  .movie-detail--ad-section-links[_ngcontent-%COMP%]   .btn[_ngcontent-%COMP%] {\n    margin-right: 1rem;\n  }\n}\n.movie-detail--ad-section-links[_ngcontent-%COMP%]    > .btn[_ngcontent-%COMP%]:last-child {\n  margin-right: 0rem;\n  float: right;\n}\n.movie-detail--basic-infos[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: space-between;\n}\n.movie-detail--cast-list[_ngcontent-%COMP%] {\n  display: flex;\n  flex-direction: row;\n  margin: 0 20px;\n  width: 100%;\n  height: 50px;\n  content-visibility: auto;\n  contain-intrinsic-size: 50px;\n  contain: strict;\n  overflow: hidden;\n}\n\n.cast-list[_ngcontent-%COMP%] {\n  width: 100%;\n  display: flex;\n  overflow-x: scroll;\n  position: relative;\n  scroll-behavior: smooth;\n  scroll-snap-type: x mandatory;\n}\n\n.cast-list[_ngcontent-%COMP%]::-webkit-scrollbar {\n  display: none;\n}\n\n.cast-list[_ngcontent-%COMP%] {\n  -ms-overflow-style: none;\n  scrollbar-width: none;\n}\n\n.cast-list--btn[_ngcontent-%COMP%] {\n  min-height: 48px;\n  min-width: 48px;\n  padding-bottom: 4px;\n  background: transparent;\n  border: 0;\n  z-index: 2;\n  font-size: 40px;\n  text-decoration: none;\n  cursor: pointer;\n  color: rgb(102, 102, 102);\n}\n\n.movie-detail--languages-runtime-release[_ngcontent-%COMP%] {\n  color: var(--palette-warning-dark);\n  text-transform: uppercase;\n}\n\n.movie-detail--section[_ngcontent-%COMP%] {\n  margin-bottom: 3rem;\n}\n\n.movie-detail--cast-actor[_ngcontent-%COMP%] {\n  display: block;\n  height: auto;\n  width: 70px;\n  content-visibility: auto;\n  contain-intrinsic-size: 50px;\n  flex-shrink: 0;\n}\n.movie-detail--cast-actor[_ngcontent-%COMP%]   picture[_ngcontent-%COMP%] {\n  display: inline-flex;\n  align-items: center;\n  width: 44px;\n  height: 44px;\n  position: relative;\n  overflow: hidden;\n  border-radius: var(--theme-borderRadius-circle);\n}\n.movie-detail--cast-actor[_ngcontent-%COMP%]   img[_ngcontent-%COMP%] {\n  display: inline;\n  margin: 0 auto;\n  object-fit: cover;\n  margin: 0 auto;\n}\n\n.recommendations[_ngcontent-%COMP%] {\n  contain: content;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9hc3BlY3QtcmF0aW8vYXNwZWN0LXJhdGlvLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9wYWdlcy9tb3ZpZS1kZXRhaWwtcGFnZS9tb3ZpZS1kZXRhaWwtcGFnZS5jb21wb25lbnQuc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9sb2FkZXIvbG9hZGVyLnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC91aS9jb21wb25lbnQvYnV0dG9uL19idXR0b24uc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL21peGlucy9fZmxleC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvY29tcG9uZW50L2RpYWxvZy9kaWFsb2cuY29tcG9uZW50LnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7RUFDRSxXQUFBO0VBQ0EsY0FBQTtFQUNBLFlBQUE7RUFDQSwwQ0FBQTtBQ0NGOztBRENBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsMkNBQUE7QUNFRjs7QURBQTtFQUNFLFdBQUE7RUFDQSxjQUFBO0VBQ0EsWUFBQTtFQUNBLDBDQUFBO0FDR0Y7O0FEQUE7RUFDRSxXQUFBO0VBQ0EsY0FBQTtFQUNBLGlCQUFBO0VBQ0EsWUFBQTtBQ0dGOztBQzFCQTtFQUNFLGFBQUE7RUFDQSxtQkFBQTtFQUNBLGdCQUFBO0VBQ0EsaUJBQUE7RUFDQSxXQUFBO0FENkJGO0FDM0JFO0VBQ0UsWUFBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0EsK0NBQUE7RUFDQSw2Q0FBQTtFQUNBLGlEQUFBO0VBQ0EsMkRBQUE7QUQ2Qko7O0FDekJBO0VBQ0Usa0JBQUE7RUFDQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLGdDQUFBO0FENEJGOztBQ3pCQTtFQUNFO0lBQ0UsWUFBQTtJQUNBLGtDQUFBO0VENEJGO0VDMUJBO0lBQ0UsVUFBQTtJQUNBLG9DQUFBO0VENEJGO0FBQ0Y7QUU1REE7RUNpQkUsYUFBQTtFQUNBLG1CQUFBO0VBQ0EsdUJBQUE7RUFoQkEsb0JBQUE7RURBQSxhQUFBO0VBQ0EsaUJBQUE7RUFDQSxlQUFBO0VBQ0EsZ0JBQUE7RUFDQSxtQkFBQTtFQUNBLHlCQUFBO0VBQ0Esa0NBQUE7RUFDQSw0REFBQTtFQUNBLDBDQUFBO0VBQ0EsZ0JBQUE7RUFDQSw2QkFBQTtFQUNBLGVBQUE7QUZnRUY7O0FFN0RBO0VBQ0UsV0FBQTtFQUNBLFlBQUE7RUFDQSxnQkFBQTtBRmdFRjs7QUU3REE7RUFDRSx1Q0FBQTtFQUNBLDJDQUFBO0FGZ0VGO0FFOURFO0VBQ0UsOENBQUE7QUZnRUo7O0FFNURBO0VBQ0UsZ0JBQUE7RUFDQSxZQUFBO0VBQ0EsY0FBQTtFQUNBLGdCQUFBO0VBQ0EsZUFBQTtBRitERjs7QUlwR0U7RUFDRSw4QkFBQTtFQUNBLG1HQUFBO0FKdUdKO0FJbkdFO0VBQ0Usa0JBQUE7RUFDQSxXQUFBO0VBQ0EsWUFBQTtFQUNBLFVBQUE7RUFDQSxZQUFBO0FKcUdKO0FJbkdJO0VBRUUsV0FBQTtFQUNBLGtCQUFBO0VBQ0EsV0FBQTtFQUNBLFdBQUE7RUFDQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLDhCQUFBO0VBQ0Esd0JBQUE7RUFDQSwwQ0FBQTtFQUNBLGdCQUFBO0FKb0dOO0FJakdJO0VBQ0UseUJBQUE7QUptR047QUkvRkU7RUFDRSwyQ0FBQTtFQUNBLFVBQUE7QUppR0o7QUk5RkU7O0VBRUUsV0FBQTtFQUNBLGNBQUE7RUFDQSxpQkFBQTtFQUNBLFlBQUE7QUpnR0o7O0FBcklBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7QUF3SUY7O0FBcklBO0VBQ0Usa0JBQUE7RUFDQSxZQUFBO0VBQ0EsVUFBQTtBQXdJRjs7QUFySUE7RUFDRSxpQkFBQTtBQXdJRjs7QUFySUE7RUFDRSxnQkFBQTtBQXdJRjtBQXRJRTtFQUNFO0lBQ0UsYUFBQTtFQXdJSjtBQUNGO0FBcklFO0VHdEJBLGFBQUE7RUFDQSxtQkFBQTtFSHVCRSxlQUFBO0FBd0lKO0FBdElJO0VHMUJGLGFBQUE7RUFDQSxtQkFBQTtFSCtCSSxpQkFBQTtFQUNBLGlCQUFBO0VBQ0EseUJBQUE7QUFxSU47QUE1SU07RUFDRSxrQkFBQTtBQThJUjtBQW5JSTtFRzlDRixhQUFBO0VIZ0RJLGtCQUFBO0FBcUlOO0FBbElJO0VBQ0Usa0JBQUE7QUFvSU47QUFuSU07RUFGRjtJQUdJLGtCQUFBO0VBc0lOO0FBQ0Y7QUFuSUk7RUFDRSxrQkFBQTtFQUNBLFlBQUE7QUFxSU47QUFqSUU7RUd6REEsYUFBQTtFQUNBLG1CQUFBO0VIMERFLDhCQUFBO0FBb0lKO0FBaklFO0VHckVBLGFBQUE7RUh1RUUsbUJBQUE7RUFDQSxjQUFBO0VBQ0EsV0FBQTtFQUNBLFlBQUE7RUFDQSx3QkFBQTtFQUNBLDRCQUFBO0VBQ0EsZUFBQTtFQUNBLGdCQUFBO0FBbUlKOztBQS9IQTtFQUNFLFdBQUE7RUFDQSxhQUFBO0VBQ0Esa0JBQUE7RUFDQSxrQkFBQTtFQUNBLHVCQUFBO0VBQ0EsNkJBQUE7QUFrSUY7O0FBL0hBO0VBQ0UsYUFBQTtBQWtJRjs7QUEvSEE7RUFDRSx3QkFBQTtFQUNBLHFCQUFBO0FBa0lGOztBQS9IQTtFQUNFLGdCQUFBO0VBQ0EsZUFBQTtFQUNBLG1CQUFBO0VBQ0EsdUJBQUE7RUFDQSxTQUFBO0VBQ0EsVUFBQTtFQUNBLGVBQUE7RUFDQSxxQkFBQTtFQUNBLGVBQUE7RUFDQSx5QkFBQTtBQWtJRjs7QUEvSEE7RUFDRSxrQ0FBQTtFQUNBLHlCQUFBO0FBa0lGOztBQS9IQTtFQUNFLG1CQUFBO0FBa0lGOztBQS9IQTtFQUNFLGNBQUE7RUFDQSxZQUFBO0VBQ0EsV0FBQTtFQUNBLHdCQUFBO0VBQ0EsNEJBQUE7RUFDQSxjQUFBO0FBa0lGO0FBaElFO0VBQ0Usb0JBQUE7RUFDQSxtQkFBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0Esa0JBQUE7RUFDQSxnQkFBQTtFQUNBLCtDQUFBO0FBa0lKO0FBL0hFO0VBQ0UsZUFBQTtFQUNBLGNBQUE7RUFDQSxpQkFBQTtFQUNBLGNBQUE7QUFpSUo7O0FBOUhBO0VBQ0UsZ0JBQUE7QUFpSUYiLCJzb3VyY2VzQ29udGVudCI6WyIuYXNwZWN0UmF0aW8tMi0zIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMi0zKTtcbn1cbi5hc3BlY3RSYXRpby0xNi05IHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMTYtOSk7XG59XG4uYXNwZWN0UmF0aW8tNC0zIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tNC0zKTtcbn1cblxuLmZpdC1jb3ZlciB7XG4gIHdpZHRoOiAxMDAlO1xuICBkaXNwbGF5OiBibG9jaztcbiAgb2JqZWN0LWZpdDogY292ZXI7XG4gIGhlaWdodDogMTAwJTtcbn1cbiIsIkBpbXBvcnQgJy4uLy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcbkBpbXBvcnQgJy4uLy4uL3VpL2NvbXBvbmVudC9hc3BlY3QtcmF0aW8vYXNwZWN0LXJhdGlvJztcbkBpbXBvcnQgJy4uLy4uL3VpL2NvbXBvbmVudC9sb2FkZXIvbG9hZGVyJztcbkBpbXBvcnQgJy4uLy4uL3VpL2NvbXBvbmVudC9idXR0b24vYnV0dG9uJztcbkBpbXBvcnQgJy4uLy4uL3VpL2NvbXBvbmVudC9kaWFsb2cvZGlhbG9nLmNvbXBvbmVudCc7XG5cbjpob3N0IHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xufVxuXG4ubG9hZGVyIHtcbiAgcG9zaXRpb246IGFic29sdXRlO1xuICB6LWluZGV4OiAyMDA7XG4gIHRvcDogMjUwcHg7XG59XG5cbi5tb3ZpZS1kZXRhaWwtd3JhcHBlciB7XG4gIG1pbi1oZWlnaHQ6IDUwMHB4O1xufVxuXG4ubW92aWUtZGV0YWlsIHtcbiAgY29udGFpbjogY29udGVudDtcblxuICBAbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDE1MDBweCkge1xuICAgICYtLWdyaWQtaXRlbSB7XG4gICAgICBwYWRkaW5nOiAzcmVtO1xuICAgIH1cbiAgfVxuXG4gICYtLWdlbnJlcyB7XG4gICAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gICAgZmxleC13cmFwOiB3cmFwO1xuXG4gICAgJi1saW5rIHtcbiAgICAgICY6bm90KDpsYXN0LWNoaWxkKSB7XG4gICAgICAgIG1hcmdpbi1yaWdodDogMnJlbTtcbiAgICAgIH1cblxuICAgICAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gICAgICBwYWRkaW5nOiAwLjVyZW0gMDtcbiAgICAgIGZvbnQtd2VpZ2h0OiBib2xkO1xuICAgICAgdGV4dC10cmFuc2Zvcm06IHVwcGVyY2FzZTtcbiAgICB9XG4gIH1cblxuICAmLS1hZC1zZWN0aW9uLWxpbmtzIHtcbiAgICAuc2VjdGlvbi0tY29udGVudCB7XG4gICAgICBAaW5jbHVkZSBkLWZsZXg7XG4gICAgICBtYXJnaW4tcmlnaHQ6IGF1dG87XG4gICAgfVxuXG4gICAgLmJ0biB7XG4gICAgICBtYXJnaW4tcmlnaHQ6IDJyZW07XG4gICAgICBAbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDEzMDBweCkge1xuICAgICAgICBtYXJnaW4tcmlnaHQ6IDFyZW07XG4gICAgICB9XG4gICAgfVxuXG4gICAgPiAuYnRuOmxhc3QtY2hpbGQge1xuICAgICAgbWFyZ2luLXJpZ2h0OiAwcmVtO1xuICAgICAgZmxvYXQ6IHJpZ2h0O1xuICAgIH1cbiAgfVxuXG4gICYtLWJhc2ljLWluZm9zIHtcbiAgICBAaW5jbHVkZSBkLWZsZXgtdjtcbiAgICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWJldHdlZW47XG4gIH1cblxuICAmLS1jYXN0LWxpc3Qge1xuICAgIEBpbmNsdWRlIGQtZmxleDtcbiAgICBmbGV4LWRpcmVjdGlvbjogcm93O1xuICAgIG1hcmdpbjogMCAyMHB4O1xuICAgIHdpZHRoOiAxMDAlO1xuICAgIGhlaWdodDogNTBweDtcbiAgICBjb250ZW50LXZpc2liaWxpdHk6IGF1dG87XG4gICAgY29udGFpbi1pbnRyaW5zaWMtc2l6ZTogNTBweDtcbiAgICBjb250YWluOiBzdHJpY3Q7XG4gICAgb3ZlcmZsb3c6IGhpZGRlbjtcbiAgfVxufVxuXG4uY2FzdC1saXN0IHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIG92ZXJmbG93LXg6IHNjcm9sbDtcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuICBzY3JvbGwtYmVoYXZpb3I6IHNtb290aDtcbiAgc2Nyb2xsLXNuYXAtdHlwZTogeCBtYW5kYXRvcnk7XG59XG5cbi5jYXN0LWxpc3Q6Oi13ZWJraXQtc2Nyb2xsYmFyIHtcbiAgZGlzcGxheTogbm9uZTtcbn1cblxuLmNhc3QtbGlzdCB7XG4gIC1tcy1vdmVyZmxvdy1zdHlsZTogbm9uZTtcbiAgc2Nyb2xsYmFyLXdpZHRoOiBub25lO1xufVxuXG4uY2FzdC1saXN0LS1idG4ge1xuICBtaW4taGVpZ2h0OiA0OHB4O1xuICBtaW4td2lkdGg6IDQ4cHg7XG4gIHBhZGRpbmctYm90dG9tOiA0cHg7XG4gIGJhY2tncm91bmQ6IHRyYW5zcGFyZW50O1xuICBib3JkZXI6IDA7XG4gIHotaW5kZXg6IDI7XG4gIGZvbnQtc2l6ZTogNDBweDtcbiAgdGV4dC1kZWNvcmF0aW9uOiBub25lO1xuICBjdXJzb3I6IHBvaW50ZXI7XG4gIGNvbG9yOiByZ2IoMTAyLCAxMDIsIDEwMik7XG59XG5cbi5tb3ZpZS1kZXRhaWwtLWxhbmd1YWdlcy1ydW50aW1lLXJlbGVhc2Uge1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS13YXJuaW5nLWRhcmspO1xuICB0ZXh0LXRyYW5zZm9ybTogdXBwZXJjYXNlO1xufVxuXG4ubW92aWUtZGV0YWlsLS1zZWN0aW9uIHtcbiAgbWFyZ2luLWJvdHRvbTogM3JlbTtcbn1cblxuLm1vdmllLWRldGFpbC0tY2FzdC1hY3RvciB7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIHdpZHRoOiA3MHB4O1xuICBjb250ZW50LXZpc2liaWxpdHk6IGF1dG87XG4gIGNvbnRhaW4taW50cmluc2ljLXNpemU6IDUwcHg7XG4gIGZsZXgtc2hyaW5rOiAwO1xuXG4gIHBpY3R1cmUge1xuICAgIGRpc3BsYXk6IGlubGluZS1mbGV4O1xuICAgIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gICAgd2lkdGg6IDQ0cHg7XG4gICAgaGVpZ2h0OiA0NHB4O1xuICAgIHBvc2l0aW9uOiByZWxhdGl2ZTtcbiAgICBvdmVyZmxvdzogaGlkZGVuO1xuICAgIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1jaXJjbGUpO1xuICB9XG5cbiAgaW1nIHtcbiAgICBkaXNwbGF5OiBpbmxpbmU7XG4gICAgbWFyZ2luOiAwIGF1dG87XG4gICAgb2JqZWN0LWZpdDogY292ZXI7XG4gICAgbWFyZ2luOiAwIGF1dG87XG4gIH1cbn1cbi5yZWNvbW1lbmRhdGlvbnMge1xuICBjb250YWluOiBjb250ZW50O1xufVxuIiwiLmxvYWRlciB7XG4gIGRpc3BsYXk6IGdyaWQ7XG4gIHBsYWNlLWl0ZW1zOiBjZW50ZXI7XG4gIG1pbi13aWR0aDogMTUwcHg7XG4gIG1pbi1oZWlnaHQ6IDE1MHB4O1xuICB3aWR0aDogMTAwJTtcblxuICAmOmFmdGVyIHtcbiAgICBjb250ZW50OiAnICc7XG4gICAgd2lkdGg6IDNyZW07XG4gICAgaGVpZ2h0OiAzcmVtO1xuICAgIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1jaXJjbGUpO1xuICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1kYXJrKTtcbiAgICBib3gtc2hhZG93OiAtNXJlbSAwIDAgdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LW1haW4pO1xuICAgIGFuaW1hdGlvbjogY2lyY2xlLWNsYXNzaWMgMXMgZWFzZS1pbi1vdXQgaW5maW5pdGUgYWx0ZXJuYXRlO1xuICB9XG59XG5cbi5sb2FkZXIuY2VudGVyLXYge1xuICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gIHRvcDogNTAlO1xuICBsZWZ0OiA1MCU7XG4gIHRyYW5zZm9ybTogdHJhbnNsYXRlKC01MCUsIC01MCUpO1xufVxuXG5Aa2V5ZnJhbWVzIGNpcmNsZS1jbGFzc2ljIHtcbiAgMCUge1xuICAgIG9wYWNpdHk6IDAuMTtcbiAgICB0cmFuc2Zvcm06IHJvdGF0ZSgwZGVnKSBzY2FsZSgwLjUpO1xuICB9XG4gIDEwMCUge1xuICAgIG9wYWNpdHk6IDE7XG4gICAgdHJhbnNmb3JtOiByb3RhdGUoMzYwZGVnKSBzY2FsZSgxLjIpO1xuICB9XG59XG4iLCJAaW1wb3J0ICcuLi8uLi90b2tlbi9taXhpbnMvZmxleCc7XG4uYnRuIHtcbiAgQGluY2x1ZGUgZC1mbGV4LXZoO1xuICBAaW5jbHVkZSBkLWlubGluZS1mbGV4O1xuICBvdXRsaW5lOiBub25lO1xuICBwYWRkaW5nOiA2cHggMTZweDtcbiAgbWluLXdpZHRoOiA5NnB4O1xuICBtaW4taGVpZ2h0OiA0OHB4O1xuICBmb250LXdlaWdodDogbm9ybWFsO1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWRhcmspO1xuICBib3JkZXI6IDFweCBzb2xpZCByZ2JhKHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluLXJnYiksIDAuNSk7XG4gIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1tKTtcbiAgYm94LXNoYWRvdzogbm9uZTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdHJhbnNwYXJlbnQ7XG4gIGN1cnNvcjogcG9pbnRlcjtcbn1cblxuLmJ0bl9faWNvbiB7XG4gIHdpZHRoOiAyNHB4O1xuICBoZWlnaHQ6IDI0cHg7XG4gIG1hcmdpbi1sZWZ0OiA4cHg7XG59XG5cbi5wcmltYXJ5LWJ1dHRvbiB7XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1jb250cmFzdC10ZXh0KTtcblxuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbGlnaHQpO1xuICB9XG59XG5cbi5mdW5jdGlvbmFsaXR5LW9ubHktYnV0dG9uIHtcbiAgYmFja2dyb3VuZDogbm9uZTtcbiAgYm9yZGVyOiBub25lO1xuICBkaXNwbGF5OiBibG9jaztcbiAgdGV4dC1hbGlnbjogbGVmdDtcbiAgaGVpZ2h0OiBpbmhlcml0O1xufVxuIiwiQG1peGluIGQtZmxleCB7XG4gIGRpc3BsYXk6IGZsZXg7XG59XG5AbWl4aW4gZC1pbmxpbmUtZmxleCB7XG4gIGRpc3BsYXk6IGlubGluZS1mbGV4O1xufVxuXG5AbWl4aW4gZC1mbGV4LXYge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LWgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC12aCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuIiwiZGlhbG9nIHtcbiAgJjo6YmFja2Ryb3Age1xuICAgIGJhY2tncm91bmQ6IHJnYmEoMCwgMCwgMCwgMC41KTtcbiAgICB0cmFuc2l0aW9uOiBvcGFjaXR5IHZhcigtLXRoZW1lLWFuaW0tZHVyYXRpb24tbGVhdmluZ1NjcmVlbilcbiAgICAgIHZhcigtLXRoZW1lLWFuaW0tZWFzaW5nLWVhc2VJbk91dCkgMG1zO1xuICB9XG5cbiAgLmNsb3NlIHtcbiAgICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gICAgd2lkdGg6IDMwcHg7XG4gICAgaGVpZ2h0OiAzMHB4O1xuICAgIHRvcDogLTMwcHg7XG4gICAgcmlnaHQ6IC0zMHB4O1xuXG4gICAgJjo6YWZ0ZXIsXG4gICAgJjo6YmVmb3JlIHtcbiAgICAgIGNvbnRlbnQ6ICcnO1xuICAgICAgcG9zaXRpb246IGFic29sdXRlO1xuICAgICAgaGVpZ2h0OiAycHg7XG4gICAgICB3aWR0aDogMTAwJTtcbiAgICAgIHRvcDogNTAlO1xuICAgICAgbGVmdDogMHB4O1xuICAgICAgYmFja2dyb3VuZDogcmdiKDI1NSwgMjU1LCAyNTUpO1xuICAgICAgdHJhbnNmb3JtOiByb3RhdGUoNDVkZWcpO1xuICAgICAgYm9yZGVyLXJhZGl1czogdmFyKC0tdGhlbWUtYm9yZGVyUmFkaXVzLW0pO1xuICAgICAgbWFyZ2luLXRvcDogLTZweDtcbiAgICB9XG5cbiAgICAmOjphZnRlciB7XG4gICAgICB0cmFuc2Zvcm06IHJvdGF0ZSgtNDVkZWcpO1xuICAgIH1cbiAgfVxuXG4gICYudmlkZW8ge1xuICAgIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMTYtOSk7XG4gICAgd2lkdGg6IDkwJTtcbiAgfVxuXG4gIGlmcmFtZSxcbiAgdmlkZW8ge1xuICAgIHdpZHRoOiAxMDAlO1xuICAgIGRpc3BsYXk6IGJsb2NrO1xuICAgIG9iamVjdC1maXQ6IGNvdmVyO1xuICAgIGhlaWdodDogMTAwJTtcbiAgfVxufVxuIl0sInNvdXJjZVJvb3QiOiIifQ== */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (MovieDetailPageComponent);

/***/ }),

/***/ 2232:
/*!********************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/bypass-src.directive.ts ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   BypassSrcDirective: () => (/* binding */ BypassSrcDirective)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);


class BypassSrcDirective {
  constructor() {
    this.element = (0,_angular_core__WEBPACK_IMPORTED_MODULE_0__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_0__.ElementRef);
  }
  set bypassSrc(src) {
    if (typeof src === 'string') {
      this.element.nativeElement.src = src;
    }
  }
  static #_ = this.ɵfac = function BypassSrcDirective_Factory(t) {
    return new (t || BypassSrcDirective)();
  };
  static #_2 = this.ɵdir = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineDirective"]({
    type: BypassSrcDirective,
    selectors: [["iframe", "bypassSrc", ""]],
    inputs: {
      bypassSrc: "bypassSrc"
    },
    standalone: true
  });
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

/***/ 746:
/*!********************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/link/a-tag.transform.ts ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addLinkTag: () => (/* binding */ addLinkTag)
/* harmony export */ });
function addLinkTag(_res, prop, options = {}) {
  let {
    target,
    baseUrl
  } = options;
  target = target || `_blank`;
  baseUrl = baseUrl || `https://www.imdb.com/title/`;
  const res = _res;
  res.href = baseUrl + _res[prop];
  res.target = target;
  return res;
}

/***/ }),

/***/ 9705:
/*!*************************************************************************!*\
  !*** ./projects/movies/src/app/shared/cdk/video/video-tag.transform.ts ***!
  \*************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   addVideoTag: () => (/* binding */ addVideoTag)
/* harmony export */ });
function addVideoTag(_res, options) {
  let {
    baseUrl
  } = options;
  const {
    pathPropFn
  } = options;
  baseUrl = baseUrl || 'https://www.youtube.com/embed';
  const res = _res;
  const path = pathPropFn(res);
  res.videoUrl = path ? `${baseUrl}/${path}` : false;
  return res;
}

/***/ }),

/***/ 9575:
/*!****************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/operators/merge.js ***!
  \****************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   merge: () => (/* binding */ merge)
/* harmony export */ });
/* harmony import */ var _util_lift__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../util/lift */ 4114);
/* harmony import */ var _util_argsOrArgArray__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../util/argsOrArgArray */ 9668);
/* harmony import */ var _mergeAll__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./mergeAll */ 7047);
/* harmony import */ var _util_args__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../util/args */ 6190);
/* harmony import */ var _observable_from__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../observable/from */ 6231);





function merge(...args) {
  const scheduler = (0,_util_args__WEBPACK_IMPORTED_MODULE_0__.popScheduler)(args);
  const concurrent = (0,_util_args__WEBPACK_IMPORTED_MODULE_0__.popNumber)(args, Infinity);
  args = (0,_util_argsOrArgArray__WEBPACK_IMPORTED_MODULE_1__.argsOrArgArray)(args);
  return (0,_util_lift__WEBPACK_IMPORTED_MODULE_2__.operate)((source, subscriber) => {
    (0,_mergeAll__WEBPACK_IMPORTED_MODULE_3__.mergeAll)(concurrent)((0,_observable_from__WEBPACK_IMPORTED_MODULE_4__.from)([source, ...args], scheduler)).subscribe(subscriber);
  });
}

/***/ }),

/***/ 9690:
/*!********************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/operators/mergeWith.js ***!
  \********************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   mergeWith: () => (/* binding */ mergeWith)
/* harmony export */ });
/* harmony import */ var _merge__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./merge */ 9575);

function mergeWith(...otherSources) {
  return (0,_merge__WEBPACK_IMPORTED_MODULE_0__.merge)(...otherSources);
}

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_movie-detail-page_movie-detail-page_component_ts.js.map