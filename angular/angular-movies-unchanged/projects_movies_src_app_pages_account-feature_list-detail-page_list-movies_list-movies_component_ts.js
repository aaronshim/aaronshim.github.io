"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_list-detail-page_list-movies_list-movies_component_ts"],{

/***/ 4895:
/*!*************************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-movies/list-movies.component.ts ***!
  \*************************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../list-detail-page.adapter */ 2951);
/* harmony import */ var _ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../../../../ui/pattern/movie-list/movie-list.component */ 4141);




class ListMoviesComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_2__.inject)(_list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_0__.ListDetailAdapter);
  }
  static #_ = this.ɵfac = function ListMoviesComponent_Factory(t) {
    return new (t || ListMoviesComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵdefineComponent"]({
    type: ListMoviesComponent,
    selectors: [["ct-list-movies"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵStandaloneFeature"]],
    decls: 1,
    vars: 1,
    consts: [[3, "movies"]],
    template: function ListMoviesComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵelement"](0, "ui-movie-list", 0);
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_2__["ɵɵproperty"]("movies", ctx.adapter.movies$);
      }
    },
    dependencies: [_ui_pattern_movie_list_movie_list_component__WEBPACK_IMPORTED_MODULE_1__.MovieListComponent],
    encapsulation: 2,
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (ListMoviesComponent);

/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_list-detail-page_list-movies_list-movies_component_ts.js.map