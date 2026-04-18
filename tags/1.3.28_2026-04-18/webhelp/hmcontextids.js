var hmContextIds = new Array();
function hmGetContextId(query) {
    var urlParams;
    var match,
        pl = /\+/g,
        search = /([^&=]+)=?([^&]*)/g,
        decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); },
    params = {};
    while (match = search.exec(query))
       params[decode(match[1])] = decode(match[2]);
    if (params["contextid"]) return decodeURIComponent(hmContextIds[params["contextid"]]);
    else return "";
}

hmContextIds["55"]="readme_txt.html";
hmContextIds["50"]="ccompatibility.html";
hmContextIds["57"]="change-log-2006-2015.html";
hmContextIds["56"]="credits.html";
hmContextIds["12"]="gexperts_configuration.html";
hmContextIds["29"]="editor_experts.html";
hmContextIds["53"]="generalconfiguration.html";
hmContextIds["30"]="ide_enhancements.html";
hmContextIds["35"]="code_editor_enhancements.html";
hmContextIds["32"]="editor_toolbar.html";
hmContextIds["21"]="ascii_chart.html";
hmContextIds["10"]="backup_project.html";
hmContextIds["5"]="class_browser.html";
hmContextIds["14"]="clean_directories.html";
hmContextIds["13"]="clipboard_history.html";
hmContextIds["17"]="code_librarian.html";
hmContextIds["20"]="code_proofreader.html";
hmContextIds["22"]="component_grid.html";
hmContextIds["33"]="components_to_code.html";
hmContextIds["43"]="copycomponentnames.html";
hmContextIds["71"]="editunitsearchpath.html";
hmContextIds["59"]="editorbookmarks00general.html";
hmContextIds["7"]="expert_manager.html";
hmContextIds["15"]="favorite_files.html";
hmContextIds["45"]="findcomponentreference.html";
hmContextIds["75"]="formatter.html";
hmContextIds["3"]="grep_results.html";
hmContextIds["1"]="grep_search.html";
hmContextIds["52"]="hideshownon-visualcomponents.html";
hmContextIds["23"]="ide_menu_shortcuts.html";
hmContextIds["44"]="macrolibrary.html";
hmContextIds["40"]="macrotemplates.html";
hmContextIds["9"]="message_dialog.html";
hmContextIds["46"]="openfile.html";
hmContextIds["16"]="pe_information.html";
hmContextIds["25"]="perfect_layout.html";
hmContextIds["4"]="procedure_list.html";
hmContextIds["6"]="project_dependencies.html";
hmContextIds["27"]="project_option_sets.html";
hmContextIds["51"]="propertyvalueconstants.html";
hmContextIds["42"]="renamecomponents.html";
hmContextIds["19"]="replace_components.html";
hmContextIds["47"]="setcomponentproperties.html";
hmContextIds["54"]="set_tab_order_2.html";
hmContextIds["11"]="set_tab_order.html";
hmContextIds["26"]="source_export.html";
hmContextIds["41"]="substitutionmacros.html";
hmContextIds["70"]="todogrid00general.html";
hmContextIds["24"]="to_do_list.html";
hmContextIds["58"]="usesclausemanager.html";
hmContextIds["60"]="editorbookmarks10edit.html";
hmContextIds["61"]="editorbookmarks20select.html";
hmContextIds["62"]="editorbookmarks30options.html";
hmContextIds["72"]="editunitsearchpathfavorites.html";
hmContextIds["73"]="editunitsearchpathfavoritesedit.html";
hmContextIds["74"]="editunitsearchpathoptions.html";
hmContextIds["66"]="todogrid10rules.html";
hmContextIds["64"]="todogrid20options.html";
hmContextIds["69"]="todogrid50fltrmain.html";
hmContextIds["68"]="todogrid60fltrsub.html";
hmContextIds["65"]="usesclausemanageroptions.html";
