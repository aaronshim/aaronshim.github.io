"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_person-detail-page_person-detail-page_component_ts"],{

/***/ 741:
/*!******************************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/resources/person.resource.ts ***!
  \******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   PersonResource: () => (/* binding */ PersonResource)
/* harmony export */ });
/* harmony import */ var _internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./internal/base-urls.constant */ 7782);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _angular_common_http__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/common/http */ 4860);




const URL_PERSON = id => `${[_internal_base_urls_constant__WEBPACK_IMPORTED_MODULE_0__.baseUrlApiV3, 'person', id].join('/')}`;
class PersonResource {
  constructor() {
    this.http = (0,_angular_core__WEBPACK_IMPORTED_MODULE_1__.inject)(_angular_common_http__WEBPACK_IMPORTED_MODULE_2__.HttpClient);
    this.getPerson = (id, params = {
      append_to_response: 'videos'
    }) => {
      return this.http.get(URL_PERSON(id), {
        params
      });
    };
  }
  static #_ = this.ɵfac = function PersonResource_Factory(t) {
    return new (t || PersonResource)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_1__["ɵɵdefineInjectable"]({
    token: PersonResource,
    factory: PersonResource.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 7810:
/*!*******************************************************************!*\
  !*** ./projects/movies/src/app/data-access/api/sort/sort.data.ts ***!
  \*******************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   SORT_VALUES: () => (/* binding */ SORT_VALUES)
/* harmony export */ });
const SORT_VALUES = [{
  name: 'Popularity',
  value: 'popularity.desc'
}, {
  name: 'Votes Average',
  value: 'vote_average.desc'
}, {
  name: 'Original Title',
  value: 'original_title.desc'
}, {
  name: 'Release Date',
  value: 'release_date.desc'
}];

/***/ }),

/***/ 3627:
/*!****************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/person-detail-page/person-detail-page.adapter.ts ***!
  \****************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   PersonDetailAdapter: () => (/* binding */ PersonDetailAdapter)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../shared/cdk/infinite-scroll/infiniteScroll */ 8762);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_router_router_state__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../shared/router/router.state */ 8202);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! rxjs */ 1891);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! rxjs */ 72);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! rxjs */ 8989);
/* harmony import */ var _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../data-access/images/image-sizes */ 4082);
/* harmony import */ var _shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../shared/cdk/image/image-tag.transform */ 9699);
/* harmony import */ var _shared_router_get_identifier_of_type_and_layout_util__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../shared/router/get-identifier-of-type-and-layout.util */ 5176);
/* harmony import */ var _state_person_state__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ../../state/person.state */ 3372);
/* harmony import */ var _data_access_api_resources_discover_resource__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! ../../data-access/api/resources/discover.resource */ 9456);













function transformToPersonDetail(_res) {
  return (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(_res, {
    pathProp: 'profile_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_2__.W300H450,
    sizes: `(min-width: 901px) 15vw, 42vw`,
    srcset: '154w, 185w, 342w, 500w, 780w'
  });
}
function transformToMovieModel(_res) {
  return (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_3__.addImageTag)(_res, {
    pathProp: 'poster_path',
    dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_2__.W300H450,
    sizes: '(min-width: 600px) 21vw, 15vw',
    srcset: '185w, 342w'
  });
}
class PersonDetailAdapter extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_7__.RxState {
  constructor() {
    super();
    this.routerState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_8__.inject)(_shared_router_router_state__WEBPACK_IMPORTED_MODULE_1__.RouterState);
    this.personState = (0,_angular_core__WEBPACK_IMPORTED_MODULE_8__.inject)(_state_person_state__WEBPACK_IMPORTED_MODULE_5__.PersonState);
    this.discoverResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_8__.inject)(_data_access_api_resources_discover_resource__WEBPACK_IMPORTED_MODULE_6__.DiscoverResource);
    this.actions = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_9__.RxActionFactory().create();
    this.paginate = this.actions.paginate;
    this.toggleSorting = this.actions.toggleSorting;
    this.sortBy = this.actions.sortBy;
    this.routerPersonId$ = this.routerState.select((0,_shared_router_get_identifier_of_type_and_layout_util__WEBPACK_IMPORTED_MODULE_4__.getIdentifierOfTypeAndLayoutUtil)('person', 'detail'));
    this.sortingModel$ = this.select((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_10__.selectSlice)(['showSorting', 'activeSorting']));
    this.routedPersonCtx$ = this.routerPersonId$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_11__.switchMap)(this.personState.personByIdCtx), (0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(ctx => {
      ctx.value && (ctx.value = transformToPersonDetail(ctx.value));
      return ctx;
    }));
    this.movieRecommendationsById$ = this.routerPersonId$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_13__.combineLatestWith)(this.routerState.select('sortBy')), (0,rxjs__WEBPACK_IMPORTED_MODULE_11__.switchMap)(([with_cast, sort_by]) => {
      return (0,_shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_0__.infiniteScroll)(options => this.discoverResource.getDiscoverMovies({
        with_cast,
        ...options,
        sort_by
      }), this.actions.paginate$, this.discoverResource.getDiscoverMovies({
        with_cast,
        page: 1,
        sort_by
      }));
    }), (0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(v => ({
      ...v,
      results: v.results?.map(transformToMovieModel)
    })));
    this.sortingEvent$ = this.actions.sortBy$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_14__.withLatestFrom)(this.routerPersonId$), (0,rxjs__WEBPACK_IMPORTED_MODULE_11__.switchMap)(([{
      value
    }, with_cast]) => {
      return (0,_shared_cdk_infinite_scroll_infiniteScroll__WEBPACK_IMPORTED_MODULE_0__.infiniteScroll)(options => this.discoverResource.getDiscoverMovies({
        with_cast,
        ...options,
        sort_by: value
      }), this.actions.paginate$, this.discoverResource.getDiscoverMovies({
        with_cast,
        page: 1,
        sort_by: value
      }));
    }), (0,rxjs__WEBPACK_IMPORTED_MODULE_12__.map)(v => ({
      ...v,
      results: v.results?.map(transformToMovieModel)
    })));
    this.connect('showSorting', this.actions.toggleSorting$);
    this.connect(this.actions.sortBy$, (_, sortBy) => ({
      showSorting: false,
      activeSorting: sortBy.name
    }));
    this.hold(this.routerPersonId$, this.personState.fetchPerson);
  }
  static #_ = this.ɵfac = function PersonDetailAdapter_Factory(t) {
    return new (t || PersonDetailAdapter)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_8__["ɵɵdefineInjectable"]({
    token: PersonDetailAdapter,
    factory: PersonDetailAdapter.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 2140:
/*!******************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/person-detail-page/person-detail-page.component.ts ***!
  \******************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/common */ 6575);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _person_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./person-detail-page.adapter */ 3627);
/* harmony import */ var _data_access_api_sort_sort_data__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../data-access/api/sort/sort.data */ 7810);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! rxjs */ 7835);
/* harmony import */ var _ui_component_detail_grid_detail_grid_component__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../ui/component/detail-grid/detail-grid.component */ 3251);
/* harmony import */ var _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../ui/pattern/movie-list/movie-list.component */ 4141);
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);










function PersonDetailPageComponent_article_0_ng_container_1_a_16_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "a", 13);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](1, " IMDB ");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](2, "fast-svg", 14);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const person_r8 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]().ngIf;
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("href", "https://www.imdb.com/person/" + person_r8.imdb_id, _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵsanitizeUrl"]);
  }
}
function PersonDetailPageComponent_article_0_ng_container_1_Template(rf, ctx) {
  if (rf & 1) {
    const _r12 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](1, "ui-detail-grid")(2, "div", 6);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](3, "img", 7);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](4, "div", 8)(5, "header")(6, "h1");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](7);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](8, "h2");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](9);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](10, "section")(11, "h3");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](12, "The Biography");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](13, "p");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](14);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](15, "section", 9);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](16, PersonDetailPageComponent_article_0_ng_container_1_a_16_Template, 3, 1, "a", 10);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](17, "button", 11);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵlistener"]("click", function PersonDetailPageComponent_article_0_ng_container_1_Template_button_click_17_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵrestoreView"](_r12);
      const ctx_r11 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵresetView"](ctx_r11.back());
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](18, "fast-svg", 12);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](19, "\u00A0 Back ");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()()()();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const person_r8 = ctx.ngIf;
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngSrc", person_r8.imgSrc)("ngSrcset", person_r8.imgSrcset)("sizes", person_r8.imgSizes)("width", person_r8.imgWidth)("height", person_r8.imgHeight)("title", person_r8 == null ? null : person_r8.name);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtextInterpolate"](person_r8.name);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtextInterpolate"](person_r8.birthday);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](5);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtextInterpolate"](person_r8.biography || "There is no synopsis available...");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngIf", person_r8.imdb_id);
  }
}
function PersonDetailPageComponent_article_0_ng_template_2_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](0, "div", 15);
  }
}
function PersonDetailPageComponent_article_0_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "article");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](1, PersonDetailPageComponent_article_0_ng_container_1_Template, 20, 10, "ng-container", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](2, PersonDetailPageComponent_article_0_ng_template_2_Template, 1, 0, "ng-template", null, 5, _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplateRefExtractor"]);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
  }
  if (rf & 2) {
    const personCtx_r4 = ctx.$implicit;
    const _r6 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵreference"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngIf", personCtx_r4 == null ? null : personCtx_r4.value)("ngIfElse", _r6);
  }
}
function PersonDetailPageComponent_header_2_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "header")(1, "h1");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](3, "h2");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](4, "Movies");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const p_r13 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtextInterpolate1"]("", (p_r13 == null ? null : p_r13.value == null ? null : p_r13.value.name) || "...", " in");
  }
}
const _c0 = function (a0) {
  return {
    selected: a0
  };
};
function PersonDetailPageComponent_div_5_li_3_Template(rf, ctx) {
  if (rf & 1) {
    const _r18 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "li", 21)(1, "button", 22);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵlistener"]("click", function PersonDetailPageComponent_div_5_li_3_Template_button_click_1_listener() {
      const restoredCtx = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵrestoreView"](_r18);
      const option_r16 = restoredCtx.$implicit;
      const ctx_r17 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵresetView"](ctx_r17.sortBy(option_r16));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const option_r16 = ctx.$implicit;
    const sorting_r14 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]().$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngClass", _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵpureFunction1"](2, _c0, option_r16.name === sorting_r14.activeSorting));
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtextInterpolate1"](" ", option_r16.name, " ");
  }
}
function PersonDetailPageComponent_div_5_Template(rf, ctx) {
  if (rf & 1) {
    const _r21 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](0, "div", 16)(1, "input", 17);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵlistener"]("click", function PersonDetailPageComponent_div_5_Template_input_click_1_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵrestoreView"](_r21);
      const ctx_r20 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵresetView"](ctx_r20.toggleSorting(true));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](2, "ul", 18);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](3, PersonDetailPageComponent_div_5_li_3_Template, 3, 4, "li", 19);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](4, "button", 20);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵlistener"]("click", function PersonDetailPageComponent_div_5_Template_button_click_4_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵrestoreView"](_r21);
      const ctx_r22 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵresetView"](ctx_r22.toggleSorting(false));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](5, " \u00A0 ");
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const sorting_r14 = ctx.$implicit;
    const ctx_r2 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("value", sorting_r14.activeSorting);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("hidden", !sorting_r14.showSorting);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngForOf", ctx_r2.sortOptions);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("hidden", !sorting_r14.showSorting);
  }
}
function PersonDetailPageComponent_ng_container_6_div_2_Template(rf, ctx) {
  if (rf & 1) {
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelement"](0, "div", 15);
  }
}
function PersonDetailPageComponent_ng_container_6_Template(rf, ctx) {
  if (rf & 1) {
    const _r26 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](1, "ui-movie-list", 23);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵlistener"]("paginate", function PersonDetailPageComponent_ng_container_6_Template_ui_movie_list_paginate_1_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵrestoreView"](_r26);
      const ctx_r25 = _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵresetView"](ctx_r25.paginate());
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](2, PersonDetailPageComponent_ng_container_6_div_2_Template, 1, 0, "div", 24);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const ctx_r23 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("movies", ctx_r23.results);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("ngIf", ctx_r23.loading);
  }
}
class PersonDetailPageComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_4__.inject)(_person_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__.PersonDetailAdapter);
    this.location = (0,_angular_core__WEBPACK_IMPORTED_MODULE_4__.inject)(_angular_common__WEBPACK_IMPORTED_MODULE_5__.Location);
    this.sortOptions = _data_access_api_sort_sort_data__WEBPACK_IMPORTED_MODULE_1__.SORT_VALUES;
    this.personCtx$ = this.adapter.routedPersonCtx$;
    this.sortingModel$ = this.adapter.sortingModel$;
    this.sortBy = this.adapter.sortBy;
    this.toggleSorting = this.adapter.toggleSorting;
    this.infiniteScrollRecommendations$ = (0,rxjs__WEBPACK_IMPORTED_MODULE_6__.merge)(this.adapter.movieRecommendationsById$, this.adapter.sortingEvent$);
    this.adapter.set({
      activeSorting: this.sortOptions[0].name,
      showSorting: false
    });
  }
  paginate() {
    this.adapter.paginate();
  }
  back() {
    this.location.back();
  }
  static #_ = this.ɵfac = function PersonDetailPageComponent_Factory(t) {
    return new (t || PersonDetailPageComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵdefineComponent"]({
    type: PersonDetailPageComponent,
    selectors: [["ct-person"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵStandaloneFeature"]],
    decls: 7,
    vars: 5,
    consts: [[4, "rxLet"], ["for", "sort"], ["class", "select-wrapper", 4, "rxLet"], [4, "rxLet", "rxLetStrategy"], [4, "ngIf", "ngIfElse"], ["loading", ""], ["detailGridMedia", ""], ["priority", "high", "alt", "poster movie", "data-uf", "hero-img", 1, "aspectRatio-2-3", "fit-cover", 3, "ngSrc", "ngSrcset", "sizes", "width", "height", "title"], ["detailGridDescription", ""], [1, "movie-detail--ad-section-links"], ["class", "btn", "target", "_blank", "rel", "noopener noreferrer", 3, "href", 4, "ngIf"], ["aria-label", "Back", 1, "btn", "primary-button", 3, "click"], ["name", "back", "size", "1em"], ["target", "_blank", "rel", "noopener noreferrer", 1, "btn", 3, "href"], ["name", "imdb", 1, "btn__icon"], [1, "loader"], [1, "select-wrapper"], ["id", "sort", "type", "text", "readonly", "", 1, "select", 3, "value", "click"], [1, "options", 3, "hidden"], ["class", "option", 3, "ngClass", 4, "ngFor", "ngForOf"], [1, "select-wrapper-overlay", 3, "hidden", "click"], [1, "option", 3, "ngClass"], [1, "functionality-only-button", 3, "click"], [3, "movies", "paginate"], ["class", "loader", 4, "ngIf"]],
    template: function PersonDetailPageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](0, PersonDetailPageComponent_article_0_Template, 4, 2, "article", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](1, "article");
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](2, PersonDetailPageComponent_header_2_Template, 5, 1, "header", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementStart"](3, "label", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtext"](4, "Sort");
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](5, PersonDetailPageComponent_div_5_Template, 6, 4, "div", 2);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵtemplate"](6, PersonDetailPageComponent_ng_container_6_Template, 3, 2, "ng-container", 3);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rxLet", ctx.personCtx$);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](2);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rxLet", ctx.personCtx$);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](3);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rxLet", ctx.sortingModel$);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_4__["ɵɵproperty"]("rxLet", ctx.infiniteScrollRecommendations$)("rxLetStrategy", "immediate");
      }
    },
    dependencies: [_angular_common__WEBPACK_IMPORTED_MODULE_5__.NgFor, _angular_common__WEBPACK_IMPORTED_MODULE_5__.NgIf, _angular_common__WEBPACK_IMPORTED_MODULE_5__.NgClass, _angular_common__WEBPACK_IMPORTED_MODULE_5__.NgOptimizedImage, _ui_component_detail_grid_detail_grid_component__WEBPACK_IMPORTED_MODULE_2__.DetailGridComponent, _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_3__.MovieListComponent, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_7__.RxLet, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_8__.FastSvgComponent],
    styles: [".btn[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  justify-content: center;\n  display: inline-flex;\n  outline: none;\n  padding: 6px 16px;\n  min-width: 96px;\n  min-height: 48px;\n  font-weight: normal;\n  font-size: var(--text-md);\n  color: var(--palette-primary-dark);\n  border: 1px solid rgba(var(--palette-primary-main-rgb), 0.5);\n  border-radius: var(--theme-borderRadius-m);\n  box-shadow: none;\n  background-color: transparent;\n  cursor: pointer;\n}\n\n.btn__icon[_ngcontent-%COMP%] {\n  width: 24px;\n  height: 24px;\n  margin-left: 8px;\n}\n\n.primary-button[_ngcontent-%COMP%] {\n  background: var(--palette-primary-main);\n  color: var(--palette-primary-contrast-text);\n}\n.primary-button[_ngcontent-%COMP%]:hover {\n  background-color: var(--palette-primary-light);\n}\n\n.functionality-only-button[_ngcontent-%COMP%] {\n  background: none;\n  border: none;\n  display: block;\n  text-align: left;\n  height: inherit;\n}\n\n.loader[_ngcontent-%COMP%] {\n  display: grid;\n  place-items: center;\n  min-width: 150px;\n  min-height: 150px;\n  width: 100%;\n}\n.loader[_ngcontent-%COMP%]:after {\n  content: \" \";\n  width: 3rem;\n  height: 3rem;\n  border-radius: var(--theme-borderRadius-circle);\n  background-color: var(--palette-primary-dark);\n  box-shadow: -5rem 0 0 var(--palette-primary-main);\n  animation: _ngcontent-%COMP%_circle-classic 1s ease-in-out infinite alternate;\n}\n\n.loader.center-v[_ngcontent-%COMP%] {\n  position: absolute;\n  top: 50%;\n  left: 50%;\n  transform: translate(-50%, -50%);\n}\n\n@keyframes _ngcontent-%COMP%_circle-classic {\n  0% {\n    opacity: 0.1;\n    transform: rotate(0deg) scale(0.5);\n  }\n  100% {\n    opacity: 1;\n    transform: rotate(360deg) scale(1.2);\n  }\n}\n.aspectRatio-2-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-2-3);\n}\n\n.aspectRatio-16-9[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-16-9);\n}\n\n.aspectRatio-4-3[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  height: auto;\n  aspect-ratio: var(--theme-aspectRatio-4-3);\n}\n\n.fit-cover[_ngcontent-%COMP%] {\n  width: 100%;\n  display: block;\n  object-fit: cover;\n  height: 100%;\n}\n\n[_nghost-%COMP%] {\n  width: 100%;\n  display: block;\n}\n\n@media only screen and (max-width: 1500px) {\n  .movie-detail--grid-item[_ngcontent-%COMP%] {\n    padding: 3rem;\n  }\n}\n.movie-detail--ad-section-links[_ngcontent-%COMP%]   .section--content[_ngcontent-%COMP%] {\n  display: flex;\n  margin-right: auto;\n}\n.movie-detail--ad-section-links[_ngcontent-%COMP%]   .btn[_ngcontent-%COMP%] {\n  margin-right: 2rem;\n}\n@media only screen and (max-width: 1300px) {\n  .movie-detail--ad-section-links[_ngcontent-%COMP%]   .btn[_ngcontent-%COMP%] {\n    margin-right: 1rem;\n  }\n}\n.movie-detail--ad-section-links[_ngcontent-%COMP%]    > .btn[_ngcontent-%COMP%]:last-child {\n  margin-right: 0rem;\n  float: right;\n}\n\n.movie-detail--section[_ngcontent-%COMP%] {\n  margin-bottom: 3rem;\n}\n\n.select-wrapper[_ngcontent-%COMP%] {\n  position: relative;\n  max-width: 300px;\n}\n.select-wrapper-overlay[_ngcontent-%COMP%] {\n  position: fixed;\n  top: 0;\n  right: 0;\n  left: 0;\n  bottom: 0;\n  margin: auto;\n  background: transparent;\n  z-index: 1000;\n  box-shadow: var(--theme-shadow-dropdown);\n}\n\n.options[_ngcontent-%COMP%] {\n  max-height: 300px;\n  overflow: auto;\n  position: absolute;\n  top: calc(100% + 8px);\n  background: var(--palette-background-paper);\n  border: 1px solid var(--palette-divider);\n  width: 100%;\n  z-index: 2000;\n}\n\n.option[_ngcontent-%COMP%] {\n  font-size: var(--text-md);\n  padding: 0 16px;\n  min-height: 36px;\n  line-height: 36px;\n  border-bottom: 1px solid var(--palette-divider);\n  cursor: pointer;\n}\n.option.selected[_ngcontent-%COMP%] {\n  background: var(--palette-action-hover);\n}\n.option[_ngcontent-%COMP%]:hover {\n  background: var(--palette-action-hover);\n}\n\n.option[_ngcontent-%COMP%]   .functionality-only-button[_ngcontent-%COMP%] {\n  width: 100%;\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9idXR0b24vX2J1dHRvbi5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL19mbGV4LnNjc3MiLCJ3ZWJwYWNrOi8vLi9wcm9qZWN0cy9tb3ZpZXMvc3JjL2FwcC9wYWdlcy9wZXJzb24tZGV0YWlsLXBhZ2UvcGVyc29uLWRldGFpbC1wYWdlLmNvbXBvbmVudC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvY29tcG9uZW50L2xvYWRlci9sb2FkZXIuc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9hc3BlY3QtcmF0aW8vYXNwZWN0LXJhdGlvLnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQ0E7RUNpQkUsYUFBQTtFQUNBLG1CQUFBO0VBQ0EsdUJBQUE7RUFoQkEsb0JBQUE7RURBQSxhQUFBO0VBQ0EsaUJBQUE7RUFDQSxlQUFBO0VBQ0EsZ0JBQUE7RUFDQSxtQkFBQTtFQUNBLHlCQUFBO0VBQ0Esa0NBQUE7RUFDQSw0REFBQTtFQUNBLDBDQUFBO0VBQ0EsZ0JBQUE7RUFDQSw2QkFBQTtFQUNBLGVBQUE7QUVFRjs7QUZDQTtFQUNFLFdBQUE7RUFDQSxZQUFBO0VBQ0EsZ0JBQUE7QUVFRjs7QUZDQTtFQUNFLHVDQUFBO0VBQ0EsMkNBQUE7QUVFRjtBRkFFO0VBQ0UsOENBQUE7QUVFSjs7QUZFQTtFQUNFLGdCQUFBO0VBQ0EsWUFBQTtFQUNBLGNBQUE7RUFDQSxnQkFBQTtFQUNBLGVBQUE7QUVDRjs7QUN2Q0E7RUFDRSxhQUFBO0VBQ0EsbUJBQUE7RUFDQSxnQkFBQTtFQUNBLGlCQUFBO0VBQ0EsV0FBQTtBRDBDRjtBQ3hDRTtFQUNFLFlBQUE7RUFDQSxXQUFBO0VBQ0EsWUFBQTtFQUNBLCtDQUFBO0VBQ0EsNkNBQUE7RUFDQSxpREFBQTtFQUNBLDJEQUFBO0FEMENKOztBQ3RDQTtFQUNFLGtCQUFBO0VBQ0EsUUFBQTtFQUNBLFNBQUE7RUFDQSxnQ0FBQTtBRHlDRjs7QUN0Q0E7RUFDRTtJQUNFLFlBQUE7SUFDQSxrQ0FBQTtFRHlDRjtFQ3ZDQTtJQUNFLFVBQUE7SUFDQSxvQ0FBQTtFRHlDRjtBQUNGO0FFMUVBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsMENBQUE7QUY0RUY7O0FFMUVBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsMkNBQUE7QUY2RUY7O0FFM0VBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxZQUFBO0VBQ0EsMENBQUE7QUY4RUY7O0FFM0VBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7RUFDQSxpQkFBQTtFQUNBLFlBQUE7QUY4RUY7O0FBaEdBO0VBQ0UsV0FBQTtFQUNBLGNBQUE7QUFtR0Y7O0FBL0ZFO0VBQ0U7SUFDRSxhQUFBO0VBa0dKO0FBQ0Y7QUE5Rkk7RURqQkYsYUFBQTtFQ21CSSxrQkFBQTtBQWdHTjtBQTdGSTtFQUNFLGtCQUFBO0FBK0ZOO0FBOUZNO0VBRkY7SUFHSSxrQkFBQTtFQWlHTjtBQUNGO0FBOUZJO0VBQ0Usa0JBQUE7RUFDQSxZQUFBO0FBZ0dOOztBQTNGQTtFQUNFLG1CQUFBO0FBOEZGOztBQTNGQTtFQUNFLGtCQUFBO0VBQ0EsZ0JBQUE7QUE4RkY7QUE1RkU7RUFDRSxlQUFBO0VBQ0EsTUFBQTtFQUNBLFFBQUE7RUFDQSxPQUFBO0VBQ0EsU0FBQTtFQUNBLFlBQUE7RUFDQSx1QkFBQTtFQUNBLGFBQUE7RUFDQSx3Q0FBQTtBQThGSjs7QUExRkE7RUFDRSxpQkFBQTtFQUNBLGNBQUE7RUFDQSxrQkFBQTtFQUNBLHFCQUFBO0VBQ0EsMkNBQUE7RUFDQSx3Q0FBQTtFQUNBLFdBQUE7RUFDQSxhQUFBO0FBNkZGOztBQTFGQTtFQUNFLHlCQUFBO0VBQ0EsZUFBQTtFQUNBLGdCQUFBO0VBQ0EsaUJBQUE7RUFDQSwrQ0FBQTtFQUNBLGVBQUE7QUE2RkY7QUEzRkU7RUFDRSx1Q0FBQTtBQTZGSjtBQTFGRTtFQUNFLHVDQUFBO0FBNEZKOztBQXhGQTtFQUNFLFdBQUE7QUEyRkYiLCJzb3VyY2VzQ29udGVudCI6WyJAaW1wb3J0ICcuLi8uLi90b2tlbi9taXhpbnMvZmxleCc7XG4uYnRuIHtcbiAgQGluY2x1ZGUgZC1mbGV4LXZoO1xuICBAaW5jbHVkZSBkLWlubGluZS1mbGV4O1xuICBvdXRsaW5lOiBub25lO1xuICBwYWRkaW5nOiA2cHggMTZweDtcbiAgbWluLXdpZHRoOiA5NnB4O1xuICBtaW4taGVpZ2h0OiA0OHB4O1xuICBmb250LXdlaWdodDogbm9ybWFsO1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICBjb2xvcjogdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LWRhcmspO1xuICBib3JkZXI6IDFweCBzb2xpZCByZ2JhKHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluLXJnYiksIDAuNSk7XG4gIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1tKTtcbiAgYm94LXNoYWRvdzogbm9uZTtcbiAgYmFja2dyb3VuZC1jb2xvcjogdHJhbnNwYXJlbnQ7XG4gIGN1cnNvcjogcG9pbnRlcjtcbn1cblxuLmJ0bl9faWNvbiB7XG4gIHdpZHRoOiAyNHB4O1xuICBoZWlnaHQ6IDI0cHg7XG4gIG1hcmdpbi1sZWZ0OiA4cHg7XG59XG5cbi5wcmltYXJ5LWJ1dHRvbiB7XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1jb250cmFzdC10ZXh0KTtcblxuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kLWNvbG9yOiB2YXIoLS1wYWxldHRlLXByaW1hcnktbGlnaHQpO1xuICB9XG59XG5cbi5mdW5jdGlvbmFsaXR5LW9ubHktYnV0dG9uIHtcbiAgYmFja2dyb3VuZDogbm9uZTtcbiAgYm9yZGVyOiBub25lO1xuICBkaXNwbGF5OiBibG9jaztcbiAgdGV4dC1hbGlnbjogbGVmdDtcbiAgaGVpZ2h0OiBpbmhlcml0O1xufVxuIiwiQG1peGluIGQtZmxleCB7XG4gIGRpc3BsYXk6IGZsZXg7XG59XG5AbWl4aW4gZC1pbmxpbmUtZmxleCB7XG4gIGRpc3BsYXk6IGlubGluZS1mbGV4O1xufVxuXG5AbWl4aW4gZC1mbGV4LXYge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LWgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC12aCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuIiwiQGltcG9ydCAnLi4vLi4vdWkvY29tcG9uZW50L2J1dHRvbi9idXR0b24nO1xuQGltcG9ydCAnLi4vLi4vdWkvY29tcG9uZW50L2xvYWRlci9sb2FkZXInO1xuQGltcG9ydCAnLi4vLi4vdWkvY29tcG9uZW50L2FzcGVjdC1yYXRpby9hc3BlY3QtcmF0aW8nO1xuQGltcG9ydCAnLi4vLi4vdWkvdG9rZW4vbWl4aW5zL2ZsZXgnO1xuXG46aG9zdCB7XG4gIHdpZHRoOiAxMDAlO1xuICBkaXNwbGF5OiBibG9jaztcbn1cblxuLm1vdmllLWRldGFpbCB7XG4gIEBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogMTUwMHB4KSB7XG4gICAgJi0tZ3JpZC1pdGVtIHtcbiAgICAgIHBhZGRpbmc6IDNyZW07XG4gICAgfVxuICB9XG5cbiAgJi0tYWQtc2VjdGlvbi1saW5rcyB7XG4gICAgLnNlY3Rpb24tLWNvbnRlbnQge1xuICAgICAgQGluY2x1ZGUgZC1mbGV4O1xuICAgICAgbWFyZ2luLXJpZ2h0OiBhdXRvO1xuICAgIH1cblxuICAgIC5idG4ge1xuICAgICAgbWFyZ2luLXJpZ2h0OiAycmVtO1xuICAgICAgQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiAxMzAwcHgpIHtcbiAgICAgICAgbWFyZ2luLXJpZ2h0OiAxcmVtO1xuICAgICAgfVxuICAgIH1cblxuICAgID4gLmJ0bjpsYXN0LWNoaWxkIHtcbiAgICAgIG1hcmdpbi1yaWdodDogMHJlbTtcbiAgICAgIGZsb2F0OiByaWdodDtcbiAgICB9XG4gIH1cbn1cblxuLm1vdmllLWRldGFpbC0tc2VjdGlvbiB7XG4gIG1hcmdpbi1ib3R0b206IDNyZW07XG59XG5cbi5zZWxlY3Qtd3JhcHBlciB7XG4gIHBvc2l0aW9uOiByZWxhdGl2ZTtcbiAgbWF4LXdpZHRoOiAzMDBweDtcblxuICAmLW92ZXJsYXkge1xuICAgIHBvc2l0aW9uOiBmaXhlZDtcbiAgICB0b3A6IDA7XG4gICAgcmlnaHQ6IDA7XG4gICAgbGVmdDogMDtcbiAgICBib3R0b206IDA7XG4gICAgbWFyZ2luOiBhdXRvO1xuICAgIGJhY2tncm91bmQ6IHRyYW5zcGFyZW50O1xuICAgIHotaW5kZXg6IDEwMDA7XG4gICAgYm94LXNoYWRvdzogdmFyKC0tdGhlbWUtc2hhZG93LWRyb3Bkb3duKTtcbiAgfVxufVxuXG4ub3B0aW9ucyB7XG4gIG1heC1oZWlnaHQ6IDMwMHB4O1xuICBvdmVyZmxvdzogYXV0bztcbiAgcG9zaXRpb246IGFic29sdXRlO1xuICB0b3A6IGNhbGMoMTAwJSArIDhweCk7XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1wYXBlcik7XG4gIGJvcmRlcjogMXB4IHNvbGlkIHZhcigtLXBhbGV0dGUtZGl2aWRlcik7XG4gIHdpZHRoOiAxMDAlO1xuICB6LWluZGV4OiAyMDAwO1xufVxuXG4ub3B0aW9uIHtcbiAgZm9udC1zaXplOiB2YXIoLS10ZXh0LW1kKTtcbiAgcGFkZGluZzogMCAxNnB4O1xuICBtaW4taGVpZ2h0OiAzNnB4O1xuICBsaW5lLWhlaWdodDogMzZweDtcbiAgYm9yZGVyLWJvdHRvbTogMXB4IHNvbGlkIHZhcigtLXBhbGV0dGUtZGl2aWRlcik7XG4gIGN1cnNvcjogcG9pbnRlcjtcblxuICAmLnNlbGVjdGVkIHtcbiAgICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWFjdGlvbi1ob3Zlcik7XG4gIH1cblxuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWFjdGlvbi1ob3Zlcik7XG4gIH1cbn1cblxuLm9wdGlvbiAuZnVuY3Rpb25hbGl0eS1vbmx5LWJ1dHRvbiB7XG4gIHdpZHRoOiAxMDAlO1xufVxuIiwiLmxvYWRlciB7XG4gIGRpc3BsYXk6IGdyaWQ7XG4gIHBsYWNlLWl0ZW1zOiBjZW50ZXI7XG4gIG1pbi13aWR0aDogMTUwcHg7XG4gIG1pbi1oZWlnaHQ6IDE1MHB4O1xuICB3aWR0aDogMTAwJTtcblxuICAmOmFmdGVyIHtcbiAgICBjb250ZW50OiAnICc7XG4gICAgd2lkdGg6IDNyZW07XG4gICAgaGVpZ2h0OiAzcmVtO1xuICAgIGJvcmRlci1yYWRpdXM6IHZhcigtLXRoZW1lLWJvcmRlclJhZGl1cy1jaXJjbGUpO1xuICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1kYXJrKTtcbiAgICBib3gtc2hhZG93OiAtNXJlbSAwIDAgdmFyKC0tcGFsZXR0ZS1wcmltYXJ5LW1haW4pO1xuICAgIGFuaW1hdGlvbjogY2lyY2xlLWNsYXNzaWMgMXMgZWFzZS1pbi1vdXQgaW5maW5pdGUgYWx0ZXJuYXRlO1xuICB9XG59XG5cbi5sb2FkZXIuY2VudGVyLXYge1xuICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gIHRvcDogNTAlO1xuICBsZWZ0OiA1MCU7XG4gIHRyYW5zZm9ybTogdHJhbnNsYXRlKC01MCUsIC01MCUpO1xufVxuXG5Aa2V5ZnJhbWVzIGNpcmNsZS1jbGFzc2ljIHtcbiAgMCUge1xuICAgIG9wYWNpdHk6IDAuMTtcbiAgICB0cmFuc2Zvcm06IHJvdGF0ZSgwZGVnKSBzY2FsZSgwLjUpO1xuICB9XG4gIDEwMCUge1xuICAgIG9wYWNpdHk6IDE7XG4gICAgdHJhbnNmb3JtOiByb3RhdGUoMzYwZGVnKSBzY2FsZSgxLjIpO1xuICB9XG59XG4iLCIuYXNwZWN0UmF0aW8tMi0zIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMi0zKTtcbn1cbi5hc3BlY3RSYXRpby0xNi05IHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tMTYtOSk7XG59XG4uYXNwZWN0UmF0aW8tNC0zIHtcbiAgd2lkdGg6IDEwMCU7XG4gIGRpc3BsYXk6IGJsb2NrO1xuICBoZWlnaHQ6IGF1dG87XG4gIGFzcGVjdC1yYXRpbzogdmFyKC0tdGhlbWUtYXNwZWN0UmF0aW8tNC0zKTtcbn1cblxuLmZpdC1jb3ZlciB7XG4gIHdpZHRoOiAxMDAlO1xuICBkaXNwbGF5OiBibG9jaztcbiAgb2JqZWN0LWZpdDogY292ZXI7XG4gIGhlaWdodDogMTAwJTtcbn1cbiJdLCJzb3VyY2VSb290IjoiIn0= */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (PersonDetailPageComponent);

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

/***/ 3372:
/*!*******************************************************!*\
  !*** ./projects/movies/src/app/state/person.state.ts ***!
  \*******************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   PersonState: () => (/* binding */ PersonState)
/* harmony export */ });
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var _shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../shared/cdk/optimized-fetch */ 3467);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../shared/cdk/loading/withLoadingEmissions */ 9135);
/* harmony import */ var _data_access_api_resources_person_resource__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../data-access/api/resources/person.resource */ 741);
/* harmony import */ var _shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../shared/cdk/get */ 2303);










class PersonState extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_4__.RxState {
  constructor() {
    (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_angular_core__WEBPACK_IMPORTED_MODULE_5__.DestroyRef).onDestroy(() => this.actionsF.destroy());
    super();
    this.actionsF = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_6__.RxActionFactory();
    this.personResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_5__.inject)(_data_access_api_resources_person_resource__WEBPACK_IMPORTED_MODULE_2__.PersonResource);
    this.actions = this.actionsF.create();
    this.fetchPerson = this.actions.fetchPerson;
    this.sortMovies = this.actions.sortMovies;
    this.personByIdCtx = id => this.select((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(({
      person: {
        value,
        loading
      }
    }) => ({
      loading,
      value: (0,_shared_cdk_get__WEBPACK_IMPORTED_MODULE_3__.pluck)(value, id)
    })));
    this.connect('person', this.actions.fetchPerson$.pipe((0,_shared_cdk_optimized_fetch__WEBPACK_IMPORTED_MODULE_0__.optimizedFetch)(id => id, id => {
      return this.personResource.getPerson(id).pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_7__.map)(result => ({
        value: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.toDictionary)([result], 'id')
      })), (0,_shared_cdk_loading_withLoadingEmissions__WEBPACK_IMPORTED_MODULE_1__.withLoadingEmission)());
    })), (oldState, newPartial) => {
      const resultState = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(oldState?.person, newPartial);
      resultState.value = (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_8__.patch)(oldState?.person?.value, resultState.value);
      return resultState;
    });
  }
  initialize(identifier) {
    this.fetchPerson(identifier);
  }
  static #_ = this.ɵfac = function PersonState_Factory(t) {
    return new (t || PersonState)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_5__["ɵɵdefineInjectable"]({
    token: PersonState,
    factory: PersonState.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 445:
/*!************************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/operators/combineLatest.js ***!
  \************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   combineLatest: () => (/* binding */ combineLatest)
/* harmony export */ });
/* harmony import */ var _observable_combineLatest__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../observable/combineLatest */ 3839);
/* harmony import */ var _util_lift__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../util/lift */ 4114);
/* harmony import */ var _util_argsOrArgArray__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! ../util/argsOrArgArray */ 9668);
/* harmony import */ var _util_mapOneOrManyArgs__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../util/mapOneOrManyArgs */ 7825);
/* harmony import */ var _util_pipe__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../util/pipe */ 2476);
/* harmony import */ var _util_args__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../util/args */ 6190);






function combineLatest(...args) {
  const resultSelector = (0,_util_args__WEBPACK_IMPORTED_MODULE_0__.popResultSelector)(args);
  return resultSelector ? (0,_util_pipe__WEBPACK_IMPORTED_MODULE_1__.pipe)(combineLatest(...args), (0,_util_mapOneOrManyArgs__WEBPACK_IMPORTED_MODULE_2__.mapOneOrManyArgs)(resultSelector)) : (0,_util_lift__WEBPACK_IMPORTED_MODULE_3__.operate)((source, subscriber) => {
    (0,_observable_combineLatest__WEBPACK_IMPORTED_MODULE_4__.combineLatestInit)([source, ...(0,_util_argsOrArgArray__WEBPACK_IMPORTED_MODULE_5__.argsOrArgArray)(args)])(subscriber);
  });
}

/***/ }),

/***/ 72:
/*!****************************************************************************!*\
  !*** ./node_modules/rxjs/dist/esm/internal/operators/combineLatestWith.js ***!
  \****************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   combineLatestWith: () => (/* binding */ combineLatestWith)
/* harmony export */ });
/* harmony import */ var _combineLatest__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./combineLatest */ 445);

function combineLatestWith(...otherSources) {
  return (0,_combineLatest__WEBPACK_IMPORTED_MODULE_0__.combineLatest)(...otherSources);
}

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_person-detail-page_person-detail-page_component_ts.js.map