"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_list-detail-page_list-items-edit_list-items-edi-13eb42"],{

/***/ 3123:
/*!*******************************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-items-edit/list-items-edit.adapter.ts ***!
  \*******************************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   ListItemsEditAdapter: () => (/* binding */ ListItemsEditAdapter)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_10__ = __webpack_require__(/*! @rx-angular/cdk/transformations */ 3751);
/* harmony import */ var _rx_angular_state__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/state */ 652);
/* harmony import */ var _rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_8__ = __webpack_require__(/*! @rx-angular/state/selections */ 8748);
/* harmony import */ var _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../..//data-access/images/image-sizes */ 4082);
/* harmony import */ var _shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../../..//shared/cdk/image/image-tag.transform */ 9699);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_9__ = __webpack_require__(/*! rxjs */ 9736);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_11__ = __webpack_require__(/*! rxjs */ 8989);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_12__ = __webpack_require__(/*! rxjs */ 3317);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_13__ = __webpack_require__(/*! rxjs */ 4520);
/* harmony import */ var rxjs__WEBPACK_IMPORTED_MODULE_14__ = __webpack_require__(/*! rxjs */ 1355);
/* harmony import */ var _data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../../../data-access/api/resources/movie.resource */ 1405);
/* harmony import */ var _pages_account_feature_list_detail_page_list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../../../pages/account-feature/list-detail-page/list-detail-page.adapter */ 2951);
/* harmony import */ var _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_7__ = __webpack_require__(/*! @rx-angular/state/actions */ 4402);
/* harmony import */ var _state_list_state__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! ../../../../state/list.state */ 7219);












class ListItemsEditAdapter extends _rx_angular_state__WEBPACK_IMPORTED_MODULE_5__.RxState {
  constructor() {
    super();
    this.state = (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_state_list_state__WEBPACK_IMPORTED_MODULE_4__.ListState);
    this.detailsAdapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_pages_account_feature_list_detail_page_list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_3__.ListDetailAdapter);
    this.moviesResource = (0,_angular_core__WEBPACK_IMPORTED_MODULE_6__.inject)(_data_access_api_resources_movie_resource__WEBPACK_IMPORTED_MODULE_2__.MovieResource);
    this.ui = new _rx_angular_state_actions__WEBPACK_IMPORTED_MODULE_7__.RxActionFactory().create();
    this.srcset = '92w, 154w, 185w, 342w, 500w, 780w';
    this.vm$ = this.select((0,_rx_angular_state_selections__WEBPACK_IMPORTED_MODULE_8__.selectSlice)(['items', 'searchResults', 'showResults', 'searchValue']), (0,rxjs__WEBPACK_IMPORTED_MODULE_9__.map)(({
      items,
      searchResults,
      showResults,
      searchValue
    }) => ({
      items: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_10__.dictionaryToArray)(items),
      searchResults: searchResults.filter(r => !items[r.id]),
      showResults,
      searchValue
    })));
    this.addMovieEvent$ = this.ui.addMovie$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_11__.withLatestFrom)(this.select('id')));
    this.deleteMovieEvent$ = this.ui.deleteMovie$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_11__.withLatestFrom)(this.select('id')));
    this.searchResponse$ = this.ui.search$.pipe((0,rxjs__WEBPACK_IMPORTED_MODULE_12__.distinctUntilChanged)(), (0,rxjs__WEBPACK_IMPORTED_MODULE_13__.filter)(Boolean), (0,rxjs__WEBPACK_IMPORTED_MODULE_14__.exhaustMap)(request => this.moviesResource.queryMovie(request)), (0,rxjs__WEBPACK_IMPORTED_MODULE_9__.map)(movies => movies.map(m => (0,_shared_cdk_image_image_tag_transform__WEBPACK_IMPORTED_MODULE_1__.addImageTag)({
      ...m,
      inList: false
    }, {
      pathProp: 'poster_path',
      dims: _data_access_images_image_sizes__WEBPACK_IMPORTED_MODULE_0__.W92H138
    }))));
    this.set({
      showResults: false,
      searchResults: [],
      searchValue: ''
    });
    this.connect(this.searchResponse$, (_, searchResults) => ({
      searchResults,
      showResults: true
    }));
    this.connect(this.detailsAdapter.listDetails$, (_, list) => ({
      id: list.id,
      items: (0,_rx_angular_cdk_transformations__WEBPACK_IMPORTED_MODULE_10__.toDictionary)(list.results || [], 'id')
    }));
    this.connect(this.ui.toggleResults$, (state, showResults) => ({
      showResults,
      searchValue: showResults ? '' : state.latestSelectedTitle,
      searchResults: []
    }));
    this.connect(this.ui.addMovie$, (_, movie) => ({
      showResults: false,
      searchValue: movie.title,
      latestSelectedTitle: movie.title
    }));
    this.hold(this.addMovieEvent$, this.state.addMovieToList);
    this.hold(this.deleteMovieEvent$, this.state.deleteMovieFromList);
  }
  static #_ = this.ɵfac = function ListItemsEditAdapter_Factory(t) {
    return new (t || ListItemsEditAdapter)();
  };
  static #_2 = this.ɵprov = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_6__["ɵɵdefineInjectable"]({
    token: ListItemsEditAdapter,
    factory: ListItemsEditAdapter.ɵfac,
    providedIn: 'root'
  });
}


/***/ }),

/***/ 2802:
/*!*********************************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-items-edit/list-items-edit.component.ts ***!
  \*********************************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @rx-angular/template/let */ 3658);
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../../shared/cdk/track-by */ 5221);
/* harmony import */ var _list_items_edit_adapter__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./list-items-edit.adapter */ 3123);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_6__ = __webpack_require__(/*! @push-based/ngx-fast-svg */ 5689);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/common */ 6575);








function ListItemsEditComponent_ng_container_0_li_7_Template(rf, ctx) {
  if (rf & 1) {
    const _r8 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "li", 9)(1, "button", 10);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function ListItemsEditComponent_ng_container_0_li_7_Template_button_click_1_listener() {
      const restoredCtx = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r8);
      const movie_r5 = restoredCtx.$implicit;
      const ctx_r7 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r7.adapter.ui.addMovie(movie_r5));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelement"](2, "img", 11);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](3, "h3", 12);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
  }
  if (rf & 2) {
    const movie_r5 = ctx.$implicit;
    const ctx_r3 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("ngSrc", movie_r5.imgSrc)("ngSrcset", ctx_r3.adapter.srcset)("width", movie_r5.imgWidth)("height", movie_r5.imgHeight)("title", movie_r5.title);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtextInterpolate"](movie_r5.title);
  }
}
function ListItemsEditComponent_ng_container_0_li_9_Template(rf, ctx) {
  if (rf & 1) {
    const _r11 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](0, "li", 13)(1, "div", 14);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](3, "button", 15);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function ListItemsEditComponent_ng_container_0_li_9_Template_button_click_3_listener() {
      const restoredCtx = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r11);
      const item_r9 = restoredCtx.$implicit;
      const ctx_r10 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"](2);
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r10.adapter.ui.deleteMovie(item_r9));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelement"](4, "fast-svg", 16);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()()();
  }
  if (rf & 2) {
    const item_r9 = ctx.$implicit;
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtextInterpolate1"](" ", item_r9.title, " ");
  }
}
function ListItemsEditComponent_ng_container_0_Template(rf, ctx) {
  if (rf & 1) {
    const _r13 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementContainerStart"](0);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](1, "fieldset")(2, "label", 1);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](3, " Add Item ");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](4, "input", 2, 3);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("focus", function ListItemsEditComponent_ng_container_0_Template_input_focus_4_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r13);
      const ctx_r12 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r12.adapter.ui.toggleResults(true));
    })("input", function ListItemsEditComponent_ng_container_0_Template_input_input_4_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r13);
      const _r2 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵreference"](5);
      const ctx_r14 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r14.adapter.ui.search(_r2.value));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](6, "ul", 4);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](7, ListItemsEditComponent_ng_container_0_li_7_Template, 5, 6, "li", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]()();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](8, "ol", 6);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](9, ListItemsEditComponent_ng_container_0_li_9_Template, 5, 1, "li", 7);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementStart"](10, "button", 8);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵlistener"]("click", function ListItemsEditComponent_ng_container_0_Template_button_click_10_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r13);
      const ctx_r15 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r15.adapter.ui.toggleResults(false));
    })("focus", function ListItemsEditComponent_ng_container_0_Template_button_focus_10_listener() {
      _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵrestoreView"](_r13);
      const ctx_r16 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵresetView"](ctx_r16.adapter.ui.toggleResults(false));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtext"](11, " \u00A0 ");
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementEnd"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelementContainerEnd"]();
  }
  if (rf & 2) {
    const vm_r1 = ctx.$implicit;
    const ctx_r0 = _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵnextContext"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("value", vm_r1.searchValue);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("hidden", !vm_r1.showResults || !vm_r1.searchResults.length);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxForOf", vm_r1.searchResults)("rxForTrackBy", ctx_r0.trackByMovieId);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxForOf", vm_r1.items)("rxForTrackBy", ctx_r0.trackByResultId);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("hidden", !vm_r1.searchResults.length);
  }
}
class ListItemsEditComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_list_items_edit_adapter__WEBPACK_IMPORTED_MODULE_1__.ListItemsEditAdapter);
    this.trackByMovieId = (0,_shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__.trackByProp)('id');
    this.trackByResultId = (0,_shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__.trackByProp)('id');
  }
  static #_ = this.ɵfac = function ListItemsEditComponent_Factory(t) {
    return new (t || ListItemsEditComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineComponent"]({
    type: ListItemsEditComponent,
    selectors: [["ct-list-items-edit"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵStandaloneFeature"]],
    decls: 1,
    vars: 1,
    consts: [[4, "rxLet"], ["for", "list-name"], ["id", "list-name", "placeholder", "Search for a movie...", "type", "text", 3, "value", "focus", "input"], ["itemInput", ""], [1, "results", 3, "hidden"], ["class", "item", 4, "rxFor", "rxForOf", "rxForTrackBy"], [1, "list"], ["class", "item selected", 4, "rxFor", "rxForOf", "rxForTrackBy"], ["type", "button", 1, "results-overlay", 3, "hidden", "click", "focus"], [1, "item"], [1, "poster", 3, "click"], ["alt", "poster movie", 1, "result-image", "gradient", 3, "ngSrc", "ngSrcset", "width", "height", "title"], [1, "title"], [1, "item", "selected"], [1, "box"], ["aria-label", "Delete item", 1, "delete", 3, "click"], ["name", "delete"]],
    template: function ListItemsEditComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵtemplate"](0, ListItemsEditComponent_ng_container_0_Template, 12, 7, "ng-container", 0);
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("rxLet", ctx.adapter.vm$);
      }
    },
    dependencies: [_angular_common__WEBPACK_IMPORTED_MODULE_3__.NgOptimizedImage, _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_4__.RxFor, _rx_angular_template_let__WEBPACK_IMPORTED_MODULE_5__.RxLet, _push_based_ngx_fast_svg__WEBPACK_IMPORTED_MODULE_6__.FastSvgComponent],
    styles: ["label[_ngcontent-%COMP%] {\n  display: block;\n  font-size: var(--text-md);\n}\n\ninput[_ngcontent-%COMP%] {\n  margin-top: 4px;\n  font-size: var(--text-md);\n  display: block;\n  padding: 10px 16px;\n  width: 100%;\n  border: 1px solid var(--palette-divider);\n  background: var(--palette-background-paper);\n  color: var(--palette-text-primary);\n  cursor: pointer;\n}\ninput[_ngcontent-%COMP%]:focus {\n  outline: 1px solid var(--palette-divider);\n}\n\nfieldset[_ngcontent-%COMP%] {\n  margin-top: 8px;\n  margin-bottom: 16px;\n  border: none;\n  position: relative;\n}\n\n.list[_ngcontent-%COMP%] {\n  display: block;\n  counter-reset: listCounter;\n  padding-bottom: 16px;\n}\n\n.item[_ngcontent-%COMP%] {\n  display: flex;\n  font-size: var(--text-md);\n  width: 100%;\n  align-items: center;\n  counter-increment: listCounter;\n  padding: 3px 0 3px 8px;\n}\n.item[_ngcontent-%COMP%]   .overview[_ngcontent-%COMP%] {\n  margin-bottom: 8px;\n}\n.item[_ngcontent-%COMP%]:before {\n  margin-right: 10px;\n  content: counter(listCounter);\n  background-color: var(--palette-custom-lightBlue);\n  border-radius: 50%;\n  color: var(--palette-text-primary);\n  width: 24px;\n  height: 24px;\n  display: inline-grid;\n  place-items: center;\n  flex-shrink: 0;\n}\n\n.box[_ngcontent-%COMP%] {\n  padding: 0 8px 0 20px;\n  flex-grow: 1;\n  border: 1px solid var(--palette-divider);\n  display: flex;\n  justify-content: space-between;\n  align-items: center;\n}\n\n.delete[_ngcontent-%COMP%] {\n  border: none;\n  background: none;\n  width: 48px;\n  height: 48px;\n  cursor: pointer;\n  border-radius: 50%;\n  display: flex;\n  justify-content: center;\n  align-items: center;\n}\n.delete[_ngcontent-%COMP%]:hover {\n  background: var(--palette-action-hover);\n}\n\n.results[_ngcontent-%COMP%] {\n  max-height: 300px;\n  overflow: auto;\n  position: absolute;\n  top: calc(100% + 8px);\n  background: var(--palette-background-paper);\n  border: 1px solid var(--palette-divider);\n  width: 100%;\n  z-index: 2000;\n}\n.results-overlay[_ngcontent-%COMP%] {\n  position: fixed;\n  top: 0;\n  right: 0;\n  left: 0;\n  bottom: 0;\n  margin: auto;\n  background: transparent;\n  z-index: 1000;\n}\n.results[_ngcontent-%COMP%]   .item[_ngcontent-%COMP%] {\n  padding-top: 4px;\n  padding-bottom: 4px;\n  border-bottom: 1px solid var(--palette-divider);\n}\n.results[_ngcontent-%COMP%]   .item[_ngcontent-%COMP%]:before {\n  display: none;\n}\n.results[_ngcontent-%COMP%]   .item[_ngcontent-%COMP%]:hover {\n  background: var(--palette-action-hover);\n  cursor: pointer;\n}\n\n.overview[_ngcontent-%COMP%] {\n  display: block;\n  color: var(--palette-text-secondary);\n}\n\n.poster[_ngcontent-%COMP%] {\n  width: 92px;\n  height: 92px;\n  overflow: hidden;\n  margin-right: 24px;\n}\n\n.title[_ngcontent-%COMP%] {\n  text-transform: none;\n  font-weight: normal;\n  color: var(--palette-text-primary);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3BhZ2VzL2FjY291bnQtZmVhdHVyZS9saXN0LWRldGFpbC1wYWdlL2xpc3QtaXRlbXMtZWRpdC9saXN0LWl0ZW1zLWVkaXQuY29tcG9uZW50LnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7RUFDRSxjQUFBO0VBQ0EseUJBQUE7QUFDRjs7QUFFQTtFQUNFLGVBQUE7RUFDQSx5QkFBQTtFQUNBLGNBQUE7RUFDQSxrQkFBQTtFQUNBLFdBQUE7RUFDQSx3Q0FBQTtFQUNBLDJDQUFBO0VBQ0Esa0NBQUE7RUFDQSxlQUFBO0FBQ0Y7QUFDRTtFQUNFLHlDQUFBO0FBQ0o7O0FBR0E7RUFDRSxlQUFBO0VBQ0EsbUJBQUE7RUFDQSxZQUFBO0VBQ0Esa0JBQUE7QUFBRjs7QUFHQTtFQUNFLGNBQUE7RUFDQSwwQkFBQTtFQUNBLG9CQUFBO0FBQUY7O0FBR0E7RUFDRSxhQUFBO0VBQ0EseUJBQUE7RUFDQSxXQUFBO0VBQ0EsbUJBQUE7RUFDQSw4QkFBQTtFQUNBLHNCQUFBO0FBQUY7QUFFRTtFQUNFLGtCQUFBO0FBQUo7QUFHRTtFQUNFLGtCQUFBO0VBQ0EsNkJBQUE7RUFDQSxpREFBQTtFQUNBLGtCQUFBO0VBQ0Esa0NBQUE7RUFDQSxXQUFBO0VBQ0EsWUFBQTtFQUNBLG9CQUFBO0VBQ0EsbUJBQUE7RUFDQSxjQUFBO0FBREo7O0FBS0E7RUFDRSxxQkFBQTtFQUNBLFlBQUE7RUFDQSx3Q0FBQTtFQUNBLGFBQUE7RUFDQSw4QkFBQTtFQUNBLG1CQUFBO0FBRkY7O0FBS0E7RUFDRSxZQUFBO0VBQ0EsZ0JBQUE7RUFDQSxXQUFBO0VBQ0EsWUFBQTtFQUNBLGVBQUE7RUFDQSxrQkFBQTtFQUNBLGFBQUE7RUFDQSx1QkFBQTtFQUNBLG1CQUFBO0FBRkY7QUFJRTtFQUNFLHVDQUFBO0FBRko7O0FBTUE7RUFDRSxpQkFBQTtFQUNBLGNBQUE7RUFDQSxrQkFBQTtFQUNBLHFCQUFBO0VBQ0EsMkNBQUE7RUFDQSx3Q0FBQTtFQUNBLFdBQUE7RUFDQSxhQUFBO0FBSEY7QUFLRTtFQUNFLGVBQUE7RUFDQSxNQUFBO0VBQ0EsUUFBQTtFQUNBLE9BQUE7RUFDQSxTQUFBO0VBQ0EsWUFBQTtFQUNBLHVCQUFBO0VBQ0EsYUFBQTtBQUhKO0FBTUU7RUFDRSxnQkFBQTtFQUNBLG1CQUFBO0VBQ0EsK0NBQUE7QUFKSjtBQU1JO0VBQ0UsYUFBQTtBQUpOO0FBUUU7RUFDRSx1Q0FBQTtFQUNBLGVBQUE7QUFOSjs7QUFVQTtFQUNFLGNBQUE7RUFDQSxvQ0FBQTtBQVBGOztBQVVBO0VBQ0UsV0FBQTtFQUNBLFlBQUE7RUFDQSxnQkFBQTtFQUNBLGtCQUFBO0FBUEY7O0FBVUE7RUFDRSxvQkFBQTtFQUNBLG1CQUFBO0VBQ0Esa0NBQUE7QUFQRiIsInNvdXJjZXNDb250ZW50IjpbImxhYmVsIHtcbiAgZGlzcGxheTogYmxvY2s7XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1tZCk7XG59XG5cbmlucHV0IHtcbiAgbWFyZ2luLXRvcDogNHB4O1xuICBmb250LXNpemU6IHZhcigtLXRleHQtbWQpO1xuICBkaXNwbGF5OiBibG9jaztcbiAgcGFkZGluZzogMTBweCAxNnB4O1xuICB3aWR0aDogMTAwJTtcbiAgYm9yZGVyOiAxcHggc29saWQgdmFyKC0tcGFsZXR0ZS1kaXZpZGVyKTtcbiAgYmFja2dyb3VuZDogdmFyKC0tcGFsZXR0ZS1iYWNrZ3JvdW5kLXBhcGVyKTtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtdGV4dC1wcmltYXJ5KTtcbiAgY3Vyc29yOiBwb2ludGVyO1xuXG4gICY6Zm9jdXMge1xuICAgIG91dGxpbmU6IDFweCBzb2xpZCB2YXIoLS1wYWxldHRlLWRpdmlkZXIpO1xuICB9XG59XG5cbmZpZWxkc2V0IHtcbiAgbWFyZ2luLXRvcDogOHB4O1xuICBtYXJnaW4tYm90dG9tOiAxNnB4O1xuICBib3JkZXI6IG5vbmU7XG4gIHBvc2l0aW9uOiByZWxhdGl2ZTtcbn1cblxuLmxpc3Qge1xuICBkaXNwbGF5OiBibG9jaztcbiAgY291bnRlci1yZXNldDogbGlzdENvdW50ZXI7XG4gIHBhZGRpbmctYm90dG9tOiAxNnB4O1xufVxuXG4uaXRlbSB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGZvbnQtc2l6ZTogdmFyKC0tdGV4dC1tZCk7XG4gIHdpZHRoOiAxMDAlO1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBjb3VudGVyLWluY3JlbWVudDogbGlzdENvdW50ZXI7XG4gIHBhZGRpbmc6IDNweCAwIDNweCA4cHg7XG5cbiAgLm92ZXJ2aWV3IHtcbiAgICBtYXJnaW4tYm90dG9tOiA4cHg7XG4gIH1cblxuICAmOmJlZm9yZSB7XG4gICAgbWFyZ2luLXJpZ2h0OiAxMHB4O1xuICAgIGNvbnRlbnQ6IGNvdW50ZXIobGlzdENvdW50ZXIpO1xuICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtY3VzdG9tLWxpZ2h0Qmx1ZSk7XG4gICAgYm9yZGVyLXJhZGl1czogNTAlO1xuICAgIGNvbG9yOiB2YXIoLS1wYWxldHRlLXRleHQtcHJpbWFyeSk7XG4gICAgd2lkdGg6IDI0cHg7XG4gICAgaGVpZ2h0OiAyNHB4O1xuICAgIGRpc3BsYXk6IGlubGluZS1ncmlkO1xuICAgIHBsYWNlLWl0ZW1zOiBjZW50ZXI7XG4gICAgZmxleC1zaHJpbms6IDA7XG4gIH1cbn1cblxuLmJveCB7XG4gIHBhZGRpbmc6IDAgOHB4IDAgMjBweDtcbiAgZmxleC1ncm93OiAxO1xuICBib3JkZXI6IDFweCBzb2xpZCB2YXIoLS1wYWxldHRlLWRpdmlkZXIpO1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWJldHdlZW47XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbi5kZWxldGUge1xuICBib3JkZXI6IG5vbmU7XG4gIGJhY2tncm91bmQ6IG5vbmU7XG4gIHdpZHRoOiA0OHB4O1xuICBoZWlnaHQ6IDQ4cHg7XG4gIGN1cnNvcjogcG9pbnRlcjtcbiAgYm9yZGVyLXJhZGl1czogNTAlO1xuICBkaXNwbGF5OiBmbGV4O1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcblxuICAmOmhvdmVyIHtcbiAgICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWFjdGlvbi1ob3Zlcik7XG4gIH1cbn1cblxuLnJlc3VsdHMge1xuICBtYXgtaGVpZ2h0OiAzMDBweDtcbiAgb3ZlcmZsb3c6IGF1dG87XG4gIHBvc2l0aW9uOiBhYnNvbHV0ZTtcbiAgdG9wOiBjYWxjKDEwMCUgKyA4cHgpO1xuICBiYWNrZ3JvdW5kOiB2YXIoLS1wYWxldHRlLWJhY2tncm91bmQtcGFwZXIpO1xuICBib3JkZXI6IDFweCBzb2xpZCB2YXIoLS1wYWxldHRlLWRpdmlkZXIpO1xuICB3aWR0aDogMTAwJTtcbiAgei1pbmRleDogMjAwMDtcblxuICAmLW92ZXJsYXkge1xuICAgIHBvc2l0aW9uOiBmaXhlZDtcbiAgICB0b3A6IDA7XG4gICAgcmlnaHQ6IDA7XG4gICAgbGVmdDogMDtcbiAgICBib3R0b206IDA7XG4gICAgbWFyZ2luOiBhdXRvO1xuICAgIGJhY2tncm91bmQ6IHRyYW5zcGFyZW50O1xuICAgIHotaW5kZXg6IDEwMDA7XG4gIH1cblxuICAuaXRlbSB7XG4gICAgcGFkZGluZy10b3A6IDRweDtcbiAgICBwYWRkaW5nLWJvdHRvbTogNHB4O1xuICAgIGJvcmRlci1ib3R0b206IDFweCBzb2xpZCB2YXIoLS1wYWxldHRlLWRpdmlkZXIpO1xuXG4gICAgJjpiZWZvcmUge1xuICAgICAgZGlzcGxheTogbm9uZTtcbiAgICB9XG4gIH1cblxuICAuaXRlbTpob3ZlciB7XG4gICAgYmFja2dyb3VuZDogdmFyKC0tcGFsZXR0ZS1hY3Rpb24taG92ZXIpO1xuICAgIGN1cnNvcjogcG9pbnRlcjtcbiAgfVxufVxuXG4ub3ZlcnZpZXcge1xuICBkaXNwbGF5OiBibG9jaztcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtdGV4dC1zZWNvbmRhcnkpO1xufVxuXG4ucG9zdGVyIHtcbiAgd2lkdGg6IDkycHg7XG4gIGhlaWdodDogOTJweDtcbiAgb3ZlcmZsb3c6IGhpZGRlbjtcbiAgbWFyZ2luLXJpZ2h0OiAyNHB4O1xufVxuXG4udGl0bGUge1xuICB0ZXh0LXRyYW5zZm9ybTogbm9uZTtcbiAgZm9udC13ZWlnaHQ6IG5vcm1hbDtcbiAgY29sb3I6IHZhcigtLXBhbGV0dGUtdGV4dC1wcmltYXJ5KTtcbn1cbiJdLCJzb3VyY2VSb290IjoiIn0= */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (ListItemsEditComponent);

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_list-detail-page_list-items-edit_list-items-edi-13eb42.js.map