module EvalLandscape
open AST

let evalCloud(place: PlacementType, scale: Dims): string =
    """
    <svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://www.w3.org/2000/svg" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:svg="http://www.w3.org/2000/svg" id="svg4956" viewBox="0 0 """ + (scale.w*scale.h|>string) + """400.09" version="1.0">
  <g id="layer1" transform="translate(-204.17 -180.96)">
    <path id="path4952" stroke-linejoin="round" d="m410.68 194.38c-18.3-4.9-36.83 0.71-49.62 12.9-6.68-8.03-15.72-14.23-26.58-17.13-27.92-7.49-56.69 9.12-64.18 37.05-0.55 2.05-0.69 4.12-0.98 6.17-25.15-2.62-49.22 13.13-55.97 38.33-6.69 24.96 5.93 50.49 28.62 60.99-1.23 2.67-2.38 5.43-3.17 8.37-7.48 27.92 9.15 56.63 37.07 64.11 13.68 3.67 27.5 1.52 38.98-4.87 6.32 13.53 18.12 24.42 33.66 28.58 22.18 5.94 44.68-3.47 56.83-21.51 4.92 3.48 10.39 6.36 16.56 8.01 27.92 7.49 56.63-9.14 64.11-37.07 3.77-14.04 1.44-28.25-5.36-39.9 6.73-6.41 11.99-14.53 14.57-24.15 7.48-27.93-9.15-56.63-37.07-64.12-2.94-0.78-5.89-1.23-8.82-1.5 1.74-24.52-14.02-47.66-38.65-54.26z" stroke="#000" stroke-width="14.79" fill="navajowhite"/>
  </g>
</svg>"""

let evalMountain(place: PlacementType, scale: Dims): string =
       """<svg
    xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:cc="http://creativecommons.org/ns#"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
    xmlns:svg="http://www.w3.org/2000/svg"
    xmlns:ns1="http://sozi.baierouge.fr"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    id="svg2"
    sodipodi:docname="gebergte.svg"
    viewBox="0 0 """ + (5*(scale.w+scale.h)|>string) + """ 200.04"
    version="1.1"
    inkscape:version="0.48.2 r9819"
  >
  <g
      id="layer1"
      inkscape:label="Laag 1"
      inkscape:groupmode="layer"
      transform="translate(-274.91 -570.55)"
    >
    <path
        id="path4326"
        inkscape:connector-curvature="0"
        style="fill:black"
        transform="translate(0 308.27)"
        d="m512.62 262.28c-1.4662-0.0229-3.0129 0.67843-3.75 1.3125h-0.0625c-12.618 14.575-25.425 29.416-38.031 44-0.9219 0.74213-1.55 1.889-2.4688 2.625-0.43-0.41-0.87-0.82-1.31-1.22-0.0276-0.0146-0.0626-0.0204-0.0937-0.0312l-0.0937-0.0937c-5.1395-5.1108-11.824-5.2659-15.75-8.8125-1.4878-1.3442-4.5766-1.5304-6.1562-1.5312-0.52655-0.00029-0.89043 0.0312-0.96875 0.0312-0.13217 0-0.20063 0.0798-0.25 0.21875-0.2184-0.0618-0.44642-0.0448-0.71875 0.0312-6.2328 6.2553-10.868 13.886-16.312 20.812-0.46485-0.40283-0.96501-0.78996-1.4688-1.1562-3.5943-2.8722-8.0009-4.4401-11.719-7.0938 1.0598-2.0287-1.7103 0.64508-1.9688 1.625-2.7447 2.6367-4.3874 7.0476-6.5625 9.7812-4.8355 9.7416-10.699 17.257-16.906 25.594-0.57778-1.1178-1.0482-2.1299-1.375-2.9375-4.3411-10.73-31.944-31.342-34.75-33.156-0.49779-0.29798-1.2786-0.39082-2.0938-0.375-0.79202 0.0154-1.6123 0.12854-2.1875 0.25-0.0707-0.0107-0.14575-0.0312-0.21875-0.0312-1.5166 0-6.1306 5.6268-13.5 16.375-6.1755 9.0068-18.402 20.65-35 33.375-7.7812 5.9657-11.5 9.4502-11.5 10.781 0 1.5696-11.611 19.824-11.562 21.906-0.57974 0.96405-0.9375 1.7537-0.9375 2.25 0 0.42477 5.2875-4.484 11.75-10.906 3.8332-3.8094 5.1939-1.2393 9.0312-5.1562 1.051-1.0727 4.2474-1.1189 5.0312-2.5938 4.1203-7.7528 10.204-15.153 16.594-20.781 2.7065-2.3838 5.6947-3.6567 8.3125-4.9688-0.88918 2.7627-1.592 5.9387-2.0312 9.3438-1.5672 12.148-1.2469-1.6872-2.9688-4.9375-0.78566-1.4831-6.3463 4.6448-13.812 14.062-1.3463 1.6982-2.1558 6.4045-3.6562 8.0625-2.0766 2.2945-12.792 5.9714-15.062 7.9375-6.8296 5.9138-5.1459 8.4718-5.8125 8.5312-0.5556 0.0495 0.22461 1.0286 1.7188 2.1562 0.85047 0.64187 27.026-14.976 31.281-17.5-17.153 10.412-25.96 18.674-23.375 18.188 7.1656-1.3486 12.59-1.5296 15.438-2.4688 3.6039-1.1888-0.79577 5.4776 7.0938 2.5938 5.7653-2.1074 8.1418-19.453 9.625-22.656 0.48772-1.0532 1.5286-6.8713 2.3125-12.938 1.0912-8.4448 1.7381-10.644 2.6875-9.375 0.80493 1.0754-1.3548 9.0969-2.9062 17.125-1.3268 6.8656-2.0646 13.745-1.9688 16.312 0.0599 1.6058-2.285 3.9433-3.2188 6.8438-1.3409 4.1649-1.3108 8.8391-0.34375 8.8125 1.1251-0.031 9.6788-13.42 9.0625-7.625-0.82313 7.7404-1.0606 14.229-0.53125 14.438 1.8266 0.71839 2.3374-0.66245 2.9688-8.0312 0.87657-10.232 2.98-16.495 8.1875-24.344 3.3991-5.1228 3.9812-6.5869 2.5-6.1875-1.1034 0.29751-3.2754 2.4091-5.0312 4.875-1.7004 2.388-3.2298 4.3066-3.4062 4.2812-0.17641-0.0254-0.14663-3.5398 0.0625-7.8125 0.48971-10.005 3.4437-25.796 0.5-29.031-0.0776-0.0853-0.13948-0.17618-0.21875-0.25 3.7258-6.4963 6.598-14.246 6.1562-14.688-1.6798-1.6798-9.9411 15.576-21.469 23-6.5162 4.1964-9.1657 5.5704-6.5 3.375 9.0999-7.4942 18.512-18.167 25.594-29.031 3.9958-6.1298 7.7112-11.429 8.2812-11.781 0.10106-0.0625 0.22913-0.055 0.34375-0.0312 0.29017 0.46524 0.72233 0.98175 1.25 1.5625 0.14601 0.33573 0.28649 0.70978 0.40625 1.125 0.68437 2.3728 0.96615 2.6908 1.0312 1.125 0.13858 0.42726 0.36318 1.087 0.53125 1.5938-0.1572 0.21924-0.28125 3.717-0.28125 9.625 0 8.5935 0.22259 12.109 0.5 7.8125 0.27741-4.2967 0.27741-11.328 0-15.625-0.0656-1.0161-0.12987-1.5715-0.1875-1.75 0.33564 1.0113 0.60283 1.7884 1.0625 3.1562 1.4017 4.1711 3.7773 13.334 5.25 20.344 1.4727 7.0098 3.2153 12.903 3.9062 13.094 0.72052 0.19937 0.49222-4.3309-0.5625-10.625l-1.8438-10.969 2.375 5.1875c2.2559 4.9247 4.7773 11.33 7.3125 18.281 0.32631 1.0743 0.68837 2.091 1.0938 3.0312 8.6422 24.238 16.961 53.751 14.469 54.125-0.4897 0.0736-5.1907-6.3943-10.438-14.375-11.232-17.085-22.838-28.365-21.656-23.844-0.86648 13.91-15.104 26.732-13.594 29.125 1.6253 2.5754 17.657-1.7986 22.281 5.9688 4.6241 7.7673 21.57 6.0265 22.719 6.875 5.5262 4.0825 5.228-7.3143-0.75-27.156-2.4296-8.0643-5.2659-16.557-8.1875-24.875 1.5703 0.80932 3.4618 1.1249 5.8125 0.78125 0.78845-0.11526-6.066-19.446-4.0625-19.812 1.5845-0.28992 12.065 18.401 12.812 18.094 1.3759 1.5316 3.8155 5.6531 5.4062 9.1562 3.0679 6.756 13.332 24.695 14.562 25.438 0.40176 0.24254-1.0125-2.7079-3.125-6.5312-2.1125-3.8234-5.2557-10.096-7-13.938l-3.1875-6.9688 3.0312 1.5625c1.6586 0.85717 19.143 23.934 19.719 23.469 1.3149-1.0623-20.795-32.988-28.812-47.625 0.85719-0.88313 1.6814-1.808 2.6875-2.4688 2.9264-2.6263 6.4707-6.181 9.6875-10.094 1.5693 1.0749 24.373 16.74 28.531 22.969 1.668 2.5237 3.3578 5.2357 5.3438 6.2188 7.0259 3.1836 6.9517 18.32 14.094 19.031 0.83686 0.29566 8.93-14.145 9.75-13.625 0.2766 0.3433 0.52632 0.6601 0.71875 0.96875-0.21898-0.97264-0.47768-2.231-0.65625-4.0625-0.74087-1.1062-10.063 10.362-10.969 9.9688-6.0725-0.93732-3.9868-12.76-10-15-2.5849-0.54071-5.0217-2.9807-7.1875-6.1875-4.4002-7.1087-8.8634-14.201-12.812-22.625 1.116 0.0759 2.306 0.005 2.7188-0.28125 0.16961-2.0555-2.5349-2.086-3.1562-3.7812-2.4842-4.009-1.777-8.8803-2.6875-13.344l0.33 0.22c2.5992 3.8983 8.5862 9.4729 10.812 11.719 4.77 4.812 7.9902 5.8296 8.9688 4.6875 1.7957 2.4695 3.4946 4.9784 4.75 7.7812 1.4651 1.1069-2.1689 3.8525 0.75 3.875 1.2649 0.2623 3.5722 0.89499 4.25-0.5625-2.0318-5.1452-5.473-9.6141-8.7188-14.062-2.0767-2.7394-3.958-5.6905-6.1562-8.3438-1.0324-1.7865-0.20942-3.7613 1.25-5 3.1396-4.311 6.2943-8.6304 9.75-12.688 1.3714-0.76553 3.6172-0.97587 4.2812-2.5312 0.51059 0.83702 1.3712 3.1436 2.0938 5.7188 1.7385 6.196 2.9066 7.5318 11.312 12.938 2.2709 1.4604 4.3545 3.1665 5.5312 4.4062 0.0514 2.2504 0.17284 4.4522 0.3125 6.4375-0.25564 0.90028-0.55853 1.9112-0.9375 2.9688-4.4611 12.449-4.5249 12.757-2.5 15.469 2.6971 3.6125 4.5105 3.9024 2.7188 0.4375-1.6955-3.2788-1.8617-4.1989 0.28125-9.75 0.61804 1.4136 1.3892 2.7577 2.0312 4.1562 1.912 3.5997 4.5411 6.8177 7.6875 9.4062 0.51002 1.344 1.1826 3.0043 2 4.75 5.3179 11.357 12.861 24.663 15.656 27.656 1.4465 1.5492 3.7841 3.296 5.1875 3.9062 2.4799 1.0783 2.4427 0.93767-0.34375-5.7188-3.4504-8.2424-8.7132-17.501-13.406-24.281 3.344 2.2244 6.7246 4.4244 10.125 6.5625 1.4992 0.877 2.8056 2.0226 4.3125 2.875 1.3499 0.63132 2.6912-0.34197 3-1.6875 0.51789-1.3432 1.8116-2.0123 2.5938-3.1562 1.1966-1.6802 1.8456-3.6288 2.4375-5.5938 0.60045-1.1257 1.1489-2.2984 1.9375-3.2812 1.758-2.8503 3.4958-5.7099 5.25-8.5625 0.11506-0.0362 0.22735-0.0922 0.34375-0.125 0.57412-0.26599 0.97042-1.2864 0.8125-1.75 9.4606-15.394 18.897-30.797 28.312-46.219 0.44593-0.30791 1.3716-1.4065 1.4375-2.125 0.65629-1.3043 1.1479-2.7759 1.9375-3.9375 1.8229-3.2055 3.9464-6.0305 6.2812-8.4375 0.21593-0.21765 0.43664-0.41559 0.65625-0.625-3.7189 6.7296-6.6071 12.861-10.594 19.719-0.92654 1.18-16.346 36.356-16.938 37.844-0.36222 4.4088 7.2331-3.227 11.281-10.5-2.9732 7.3817-7.5527 17.412-7.6875 20.812 0.0493 6.6628 10.705-0.27547 10.281 6.375-0.30192 1.5294 1.2335 0.28278 1.4062-0.46875 0.57191-1.5359 0.35813-3.2218 0.6875-4.8125 0.39902-2.8705 0.5244-5.808 1.0312-8.6562 0.74509-2.3153 2.3577-4.2633 2.8125-6.6875 1.0624-4.1077 2.195-8.1967 3.3438-12.281-0.47071 5.2881-0.44881 10.646 0.46875 15.875-0.84012 2.2715-1.6978 4.5386-2.5312 6.8125-1.3189 0.71884 0.16023 2.9114 1.375 2.25 1.2477-1.4659 1.5113-3.3938 2-5.2188 1.046 3.6865 2.9528 7.2183 6.0625 9.5312 1.2574 0.92974 3.075 1.6759 4.5625 0.84375 1.3655-0.8871 0.96012-3.2685-0.5625-3.75-1.3114-0.88271-1.6277-2.6588-1.1875-4.0938 0.49383-3.1185 0.11699-6.3505 1.0312-9.4062 0.63695-3.0519 2.0142-5.8689 3.1562-8.75 0.5499-1.3559 2.2482-2.1493 3.4688-1.125 1.1094 0.86035 2.1471 2.2421 3.7188 2.0938 1.8822-0.0871 3.1275-1.8814 3.4688-3.5938 1.1846-4.719-0.40345-9.6194-2.625-13.781-2.2382-4.2219-4.813-8.2306-7.1875-12.375-1.9422-6.3668-3.0306-12.576-3.625-11.938 0.32258-0.54079 0.70423-1.0386 1.125-1.5 2.3188 6.0881 3.6522 15.97 7.2812 17.5 2.0191 0.82077 3.9941 1.7991 5.9062 2.8438 1.4922 1.5858-0.52036 3.2965-1.7188 4.1875-0.69075 1.9094 1.8624 2.9869 3.1875 1.75 1.9392-0.97945 3.3205-3.7292 1.8125-5.625-2.1865-3.2078-5.1475-7.4062-8.4688-9.0938-2.0842-0.87025 1.9913-16.083-0.15625-16.781-1.9581 0.90831-6.049-8.1868-4.8438-10.031 0.41172-0.75585 3.8717 1.0606 8.1562 4.0938-1.4646-0.36747-5.7187-2.7301-5.2812-1.375 0.15886 1.6088 2.4753 4.4114 3.75 7.75 1.717 4.4971 0.35773 11.882 0.90625 13.969 3.0437 9.0665 11.951 12.818 18.188 20.062 2.1363 2.4632 4.7248 5.0041 4.9375 8.4688 0.42058 2.6831 0.30871 5.5148 1.4688 8.0312-0.35445-2.7718 0.56607-5.5309 0.40625-8.3125 0.18362-3.9756 0.40623-8.4468-2.2812-11.719-3.74-5.0758-5.2968-13.554-8.8438-18.781-2.2064-3.2514-3.5314-7.5463-5-11.531 1.2132 1.0745 2.3847 2.1704 3.5 3.2812 5.5933 5.5707 8.9409 15.013 9.5 16.906 0.92946-0.004 11.561 8.5074 23 17.625-1.0518-0.49824-2.0941-0.97344-3.1875-1.375l-0.0937 0.25c-0.2183 1.4592-0.20852 3.4458-1.8125 4.125-1.6944 0.69347-3.2374 1.7043-4.6875 2.8125-0.53818 1.4442 0.87997 2.5024 1.9062 3.2188 2.409 1.7382 3.6222 4.9082 6.5625 5.9375 3.4516 1.915 7.4776 3.4548 9.5312 7.0625 0.96005 1.5947 1.6874 3.3502 2.4062 5.0625-0.28242-1.9739 1.4979-3.7414 0.8125-5.7188l-1.05-8.63c-1.1328-1.3139-0.89057-3.1385-1.25-4.7188-0.36857-0.44608-0.66009-0.95362-0.90625-1.4688 11.82 9.3362 22.346 17.359 21.469 15-0.73216-1.9683-57.785-55.252-60.062-58.531-1.5291-1.2096-4.3303-3.1914-7.4688-5.2812-0.0121-0.008-0.0191-0.0232-0.0312-0.0312-0.1843-0.28073-0.54908-0.45911-0.96875-0.625-6.354-4.1725-13.847-8.5606-16.094-8.2812-0.40644-0.18432-0.64243-0.28642-0.8125-0.21875-0.51021 0.20301-0.66371 1.888-4.6875 7.125-1.2147 1.581-2.8428 3.5596-4.4688 5.5312-0.76794 0.67986-1.5148 1.3865-2.25 2.125-1.0583 1.1486-2.0212 2.4963-2.9375 3.875-3.5093 6.771 1.1279-3.2844-4.3125 5.8438-0.0756 0.5415-0.21148 1.0878-0.3125 1.625-2.0052 4.1679-5.183 11.457-6.6875 12.5-2.0931 1.7805-4.5758 6.3992-7.0938 7.6562-1.9057-0.17437 0.0806 0.43199-0.8125 1.375-2.3803 6.0106-4.246 10.227-7.4688 15.844-2.8443 5.1408-6.449 9.9805-10.031 14.812-2.3325 1.0411-4.8826 1.603-7.4062 1.1875-2.6752-4.4177-5.1711-8.9589-7.6875-13.469-3.5838-5.3231-8.8382-9.143-13.219-13.75-3.1527-3.117-6.2801-6.2614-9.4688-9.3438 0.89986-1.6383 1.784-3.6704 2.5625-4.0938 3.5711-4.1621 7.0937-8.3741 10.812-12.406-0.0475 0.46469-0.0994 0.92616-0.15625 1.375-0.0646 1.5908 0.76012 3.2415 1.875 3.4062-0.10634 0.22334-0.17172 0.38874-0.3125 0.65625 0.13358-0.22854 0.26832-0.44001 0.40625-0.65625 0.0437 0.003 0.0805 0.0329 0.125 0.0312 0.22332 0.0374-3.911 7.7502-3.6875 7.7188l4.375-7.7188c3.2868-0.87767 4.1905 18.781 6.9062 16.062 1.0602-1.1468 4.0189-24.654 4.9375-26.031 0.7519-0.74443 1.9014-0.08 2.1562 1.1562 0.52118 1.7461 1.3973 3.5677 2.75 4.0312 0.6425 0.11749 1.175 0.66872 1.5312 1.4375-0.6297-1.2638-0.27261-3.1134-1.0625-4.25-0.88227-0.79062-1.4527-2.2376-1.4062-3.75 0.009-1.4976 0.19573-2.9913 0.34375-4.4688 0.0572-0.40509 0.1361-0.81735 0.21875-1.2188 0.1034-0.0648 0.17025-0.0843 0.28125-0.15625 1.1463-1.3804 2.1312-1.171 3-0.34375 0.11274 1.176 0.22194 2.3382 0.28125 3.5312-0.24716 5.1277-0.74784 10.214-1.1875 15.281-0.32251 5.1622-1.5476 9.8077-2.9375 14.062-0.26829 1.248-1.1003 2.031-1 3.5625 0.13448 0.51877 0.4114-0.0791 0.5-0.375 1.012-2.383 1.9163-4.9415 2.7812-7.5625 1.5451-4.9149 3.8808-8.7557 4.9375-14.312 0.36783-3.3699 1.0635-6.516 1.7812-9.625 3.7552-2.4409 12.451-11.638 13.375-9.75 4.5713 9.3397 1.4141 4.2576 9.8125 12.625 3.4794 5.7659 0.91173-1.2681-0.21875-3.8438-5.5598-5.8214 0.96327 1.0995-4.5312-4.7812-4.435-10.177-11.164-17.201-15.719-19.719-0.096-0.0531-0.24959-0.1197-0.375-0.125-0.58242-0.51147-1.3045-0.73817-2.0625-0.75zm80.469 41.844c-0.10451 0.00041-0.0893 0.0862 0.0625 0.3125-0.0125-0.0555-0.026-0.18873-0.0625-0.3125zm-133.31 68.969c0.73612 3.2697 1.39 2.2295 0 0zm55.25-109.06c0.0207 0.0252 0.0403 0.0361 0.0625 0.0625 3.1844 3.7886 11.501 12.561 10.688 12.344-0.78726-0.21061-6.5703-0.055-7.3438-1.4688-0.71031-1.2984-3.27-1.5999-4.1875-3.7188-0.78971-1.8236 0.70154-5.4255 0.78125-7.2188zm-6.4062 0.4375c0.038 0.0188 0.0738 0.0491 0.125 0.0625-0.0505-0.006-0.10432-0.005-0.15625 0 0.0118-0.0202 0.0195-0.0422 0.0312-0.0625zm-3.625 5.25c-1.6654 3.6095-3.3437 8.0415-4.5938 9.875 3.0436-2.0288 10.918-6.5825 9-5.3438-3.0306 2.2428-9.5963 6.2351-12.656 8.4375-0.47065-1.5747 0.99137-2.7469 1.75-3.9375 1.8099-3.2982 4.2122-6.1126 6.5-9.0312zm-4.5938 9.875c-1.0715 0.71423-1.55 1.1234-0.78125 0.84375 0.23314-0.0982 0.49358-0.42183 0.78125-0.84375zm56.25 4.125c0.63632 0.0315 1.2008 0.33093 1.7188 0.8125-0.031 0.0718-0.0529 0.15157-0.0937 0.21875-0.066 0.18246-0.0967 0.3847-0.0937 0.5625-0.53122-0.48619-1.0383-1.0718-1.5312-1.5938zm-59.38 1.28c-2.6706 1.875-6.579 5.6213-9.9062 11.812-0.18592 0.34596-0.2898 0.61469-0.375 0.84375-0.17535-0.60259-0.19841-1.2822 0.125-1.8125 0.17062-0.46914 0.0772-0.94695-0.125-1.3125 1.001-1.0396 2.0213-2.0607 3.0625-3.0625 2.7802-1.5159 3.2422-5.2568 6.1562-6.25 0.40009-0.0894 0.73812-0.17413 1.0625-0.21875zm2.4688 0.15625c0.282 0.0965 0.50849 0.2132 0.71875 0.3125-0.14517 0.27049-0.26935 0.53476-0.4375 0.78125-0.77201 1.3524-1.7177 2.0777-2.7188 2.6562 1.066-1.5285 1.9045-2.8064 2.4375-3.75zm59.719 5.0625c-0.21064 0.57455-0.33745 2.2888-0.4375 5.5625-1.2054 1.426-2.3249 2.9281-3.3125 4.5 0.81796-2.549 1.6892-5.1047 2.5938-7.625 0.33312-0.82908 0.70814-1.6554 1.1562-2.4375zm0.65 11.03c-0.603 2.0154-3.3465 13.435-4.375 14.062-0.27192 1.8594-0.11888 4.0876 1.5312 5.2812 0.46364 0.65466 6.2812-5.9302 6.0625-5.8125-2.4536 3.3542-5.2792 8.5394-6.1875 10.062-0.0179-0.3895-0.15031-0.67653-0.625-0.5-2.0861 1.0286-2.5829 3.5175-3.375 5.5 0.0195-0.35346 0.0219-0.70875 0.0312-1.0625l0.79-9.53c-0.54244-1.8695 0.81063-3.5253 1.2812-5.25 0.50798-1.1605 3.5012-12.811 4.875-12.75zm-111.97 2.5625 4.8125 1.375c-0.19579 0.28905-0.13925 0.32501 0.0625 0.0312l2.6562 0.75c3.7969 1.094 9.143 3.992 7.6875 3.8438l3.0938 0.3125c0.078 0.40235 0.26794 0.79803 0.53125 1.1562-0.2805 0.2328-0.5196 0.53588-0.75 0.875-0.0903-0.14704-0.23812-0.2942-0.40625-0.46875-1.2836-1.6995-5.4127-1.899-6.8438-2.5312-3.2734-0.1151-6.4264-1.1688-9.5625-2.4062-0.0306-0.0121-0.0632-0.0192-0.0937-0.0312l-1.1875-2.9062zm1.2812 2.9375c-0.0136 0.006 0.0388 0.0195 0.28125 0.0312 0.14145-0.006 0.17065-0.0265 0.21875-0.0312-0.15086-0.005-0.41975-0.008-0.5 0zm1.75-0.28125c-0.0229 0.00058-0.0396 0.0365-0.0625 0.0625 0.17872 0.0412 0.1312-0.0642 0.0625-0.0625zm-0.43 4.53c0.89951 0.26958 1.8378 0.14549 2.6875 0.125 1.4027 3.415 3.9793 7.068 9.0938 7.8438 0.3378 1.0067 1.0464 3.1026 1.75 2.125-0.0144 0.32324-0.0222 0.64037-0.0312 0.96875-1.5652-1.1615-3.686-2.5678-5.7812-3.8125-5.2913-3.1435-6.7927-4.3542-7.7188-7.25zm127.72 2.9375 1.1562 5.0938c-0.20946-0.77245-1.5689-1.319-2.2188-1.7812-0.93198-0.66304-0.58637-0.85297 0.0312 2.625 0.009 2.7564 1.3914 4.6811 1.125 7.4375-0.69646 4.5257-5.0487 4.9386-7.375 9-1.9321 2.4644 1.4492 13.894-1.375 13.281-1.4298-0.39949-6.8643-7.2266-6.5-5.0938 0.23142 1.3166 1.5381 1.9414 2.6875 2.25 5.2515 2.4964-3.8863 20.62-0.21875 25.094 1.4035 1.6675 2.6704 3.4656 3.875 5.2812 0.58074 2.0987-1.9932 2.6455-3.4688 2.875-1.5013 1.3671 0.25109 3.5702 2 3.0938 2.1722 0.0376 4.6689-1.776 4.2188-4.1562-0.43871-3.8572-3.1611-6.8648-5.3125-9.9062-1.4381-1.7416 10.637-17.694 9.0625-19.312-2.1558-0.10942-3.0825-3.4615-1.1562-4.5312 1.3418-0.89254 10.072 6.7554 13.375 17.344 1.4234 4.5629 1.4444 9.9747 1.1562 14 1.1802-2.8343 3.0505-5.3155 1.8438-15.156-0.3029-2.47-9.5222-13.179-10.469-20.75-1.1168-8.9323 6.8044-6.1459 5.3125-12.406-2.4062-10.097-4.6938-8.4352-5.4375-9.4062-0.23476-0.28135-1.9151-4.7907-2.3125-4.875zm-164.12 0.84375c1.4351 0.33552 2.9703 0.59005 3.4062 0.96875 4.2081 2.376 6.9648 4.4954 9.5625 7.75-3.144-2.262-7.2543-4.6015-12.5-6.7188-0.008-0.0314-0.0226-0.0624-0.0312-0.0937-0.1631-0.58839-0.28247-1.2558-0.4375-1.9062zm56.875 1.0312c1.5247 1.7017 2.9928 3.4526 4.3125 5.3125l-1.53-1.09c-0.50167-0.35888-1.6134-2.2582-2.7812-4.2188zm-59.5 2.3125c0.04-0.28654 1.7339 3.3516 1.6562 5.1562-0.55705-1.5237-1.1529-3.0054-1.7812-4.375 0.0725-0.1253 0.14294-0.25229 0.21875-0.375-0.0794-0.2383-0.0997-0.36343-0.0937-0.40625zm199.94 0.78125c-0.0279 0.008-0.0277 0.0551 0.0625 0.0625 0.0213-0.0399-0.0346-0.0706-0.0625-0.0625zm-200.19 0.25c0.69907 2.8466-2.2039 7.58-5.1562 11.5 2.46-4.0098 3.307-8.1732 5.1562-11.5zm-55.35 2.91c0.74469 1.0193 1.4981 2.0826 2.2812 3.0625 3.3556 4.2365 4.9887 10.383 6.2188 16.688-3.0973-7.7715-6.0284-14.592-8.5-19.75zm122.25 3.6562c0.37583 0.7084 0.70451 1.4219 1.0312 2.1562-1.7096 2.1821-3.6085 7.0699-3 11.906 0.56898 4.5226 3.7383 8.6877 5.875 14.156 1.2829 3.2835-8.0558-10.875-8-15.625 0.0206-1.7587 2.4122-6.5674 3.5938-10.312 0.26133-0.8283 0.41897-1.6031 0.5-2.2812zm109.54 0.53c-0.32895-0.0675-0.32585 1.3386 0.1875 4.25 0.44326 2.5139 0.34831 6.1712-0.21875 8.125-1.2629 4.3512 1.4921 10.678 3.8125 8.75 0.75069-0.62361 1.2835-2.538 1.1875-4.25-0.16085-2.8701 0.0208-3.061 2.125-2.5625 1.2551 0.29737 2.9446 0.19304 3.75-0.25 1.9164-1.0542 2.0119-0.80248 3.4688 8.625 1.0414 6.7389 0.97354 7.8918-0.53125 7.0938-1.6829-0.89255-1.8048-0.82084-1.5938 0.9375 0.12386 1.0318 1.0414 2.7474 2.0312 3.8125 1.2029 1.2943 2.2578 1.5564 3.1562 0.8125 1.6239-1.3446-0.15695-17.294-2.5938-23.312l-1.625-4.0625-2.3125 2c-1.919 1.6801-2.751 1.8018-4.9688 0.625-2.2163-1.176-2.8955-2.1944-4-6-0.86596-2.9837-1.546-4.5263-1.875-4.5938zm-31.438 0.75c-0.88563 3.6948-1.7216 7.4118-2.7812 11.062-0.39808 1.2106-0.86372 3.8798-1.1875 4.1875-0.17543-1.7934-0.10923-3.5886 0.0312-5.375 1.1657-3.3472 2.4248-6.6698 3.9375-9.875zm54.906 2.1875c0.037-0.0119 0.0816-0.0142 0.125 0 3.9178 1.3306 6.2001 5.009 9.1562 7.6562 1.1026 1.0857 2.3047 2.1098 3.375 3.2188-1.4968 0.61635-2.3074-1.3617-3.375-2.0312-3.0605-2.9689-5.9159-6.1598-9.4688-8.5625 0.005-0.11312 0.0766-0.2455 0.1875-0.28125zm-129.72 4.5938c1.0092 2.8149 1.9447 5.6579 2.875 8.5-1.1474-1.8097-2.0711-3.5691-2.375-5.0625-0.22594-1.1104-0.36897-2.2851-0.5-3.4375zm-16.594 5.3125c0.0193 0.18667 0.0359 0.37532 0.0625 0.5625-0.0591-0.0954-0.13371-0.18366-0.1875-0.28125 0.0409-0.1024 0.083-0.17628 0.125-0.28125zm42.844 11.5c-1.0028 1.362-2.0046 2.7191-2.9688 4.0938-0.25769-0.59599-0.53946-1.1635-1.0312-1.5938-0.23844-0.37139-0.45279-0.75138-0.6875-1.125 0.48826 0.0406 0.9896 0.0469 1.4688-0.125 1.1325-0.26811 2.1765-0.75014 3.2188-1.25zm-202.47 13.875c0.40113-0.1583-0.70546 0.82151-3.0312 2.8125-4.2356 3.6258-8.2746 7.3327-11.938 10.875 3.8767-4.6048 7.4229-8.653 8.9375-9.6875 3.7213-2.5417 5.6301-3.8417 6.0312-4z"
    />
  </g
  >
</svg
>"""

let evalIsland(place: PlacementType, scale: Dims): string =
   """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://www.w3.org/2000/svg" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:svg="http://www.w3.org/2000/svg" id="svg2" viewBox="0 0 """ + (scale.w+scale.h|>string) + """500.4" version="1.1">
  <g id="layer1" transform="translate(-134.38 -367.11)">
    <g id="g2844" stroke="#fff">
      <path id="path2830" stroke= "black" stroke-width="3" fill="navajowhite" d="m139.65 687.44c13.64 8.5251 28.465 15.134 44.208 18.55 26.359 6.8282 53.551-0.38181 80.3 1.5058 13.819-1.4779 26.241 5.3691 39.689 6.1935 14.694 0.3496 29.575-1.2672 43.997 2.464 16.997 3.257 35.34 7.0121 51.78-0.60546 14.888-5.4115 27.406-16.018 38.159-27.338 7.0398-12.22-7.3407-22.319-17.257-26.485-20.264-11.809-43.043-18.259-64.677-27.023-21.374-7.9775-43.842-12.543-64.929-21.278-16.514-6.2532-34.585-3.5371-51.513-0.75466-26.744 5.5535-49.839 21.176-71.507 37.075-13.252 6.3779-29.947 14.227-32.014 30.554-0.0831 2.8232 1.24 5.7272 3.7651 7.1413z"/>
      <path id="path2838" stroke= "black" stroke-width="3.75" fill="navajowhite" d="m233.87 644.75c-16.974-88.192 10.812-164.61 79.888-222.81 8.3506-0.6941 7.2658 7.6222 1.203 12.283-55.149 60.433-66.026 137.2-58.867 216.09-6.5822-5.3196-13.304-10.051-22.223-5.5558z"/>
      <path id="path2854-1" stroke= "black" stroke-width="3.75" fill="navajowhite" d="m320.75 428.03c13.42 12.895 23.644 27.857 33.441 43.499 2.0715-3.4692 4.143-6.9384 6.2145-10.408l3.3195 13.159c20.373 8.2433 40.746 16.487 61.12 24.73-12.023-18.944-24.045-37.888-36.068-56.833-5.5428 0.41601-11.086 0.83201-16.628 1.248 0.0801-3.0896 1.1959-7.9138 1.2759-11.003-15.402-7.5037-31.348-13.708-48.572-10.261-1.4153 0.46189-3.6934 0.27803-4.6035 1.118 0.1672 1.5834 0.33441 3.1669 0.50162 4.7503z"/>
      <path id="path2854-4" stroke= "black" stroke-width="3.75" fill="navajowhite" d="m312.71 428.49c-16.092 9.3507-29.567 21.467-42.789 34.345-1.1913-3.861-2.3825-7.722-3.5738-11.583-2.1136 3.9997-4.2271 7.9994-6.3407 11.999-21.746 3.1855-43.491 6.371-65.237 9.5565 16.166-15.559 32.332-31.118 48.498-46.678 5.2867 1.7165 10.573 3.433 15.86 5.1494 0.65371-3.0207 0.71178-7.972 1.3655-10.993 16.741-3.6439 33.702-5.8961 49.621 1.5309 1.2657 0.78386 3.5226 1.1446 4.208 2.1762-0.53734 1.4988-1.0747 2.9977-1.612 4.4965z"/>
      <path id="path2854" stroke= "black" stroke-width="3.9928" fill="navajowhite" d="m315.14 429.34c-4.4623 19.307-12.56 36.821-21.511 54.316h12.907l-10.218 10.218c3.5852 23.125 7.1704 46.249 10.756 69.374 10.756-21.332 21.511-42.664 32.267-63.996-3.4059-4.84-6.8119-9.68-10.218-14.52 2.8682-1.6133 7.8874-3.2267 10.756-4.84-1.5477-18.176-4.5806-36.14-17.134-50.005-1.1948-1.0417-2.2703-3.2247-3.535-3.5982l-4.0688 3.0516z"/>
      <path id="path2854-9" stroke= "black" stroke-width="3.3861" fill="navajowhite" d="m312.88 425.14c-15.78-5.7803-31.97-8.1472-48.544-9.8999 1.9942-3.0553 3.9883-6.1106 5.9825-9.1658-3.9975 0.84005-7.995 1.6801-11.992 2.5202-14.76-13.265-29.521-26.529-44.281-39.794 20.134 2.2495 40.269 4.499 60.403 6.7486 1.8585 4.6622 3.717 9.3244 5.5754 13.987 2.4752-1.289 5.9474-4.1057 8.4226-5.3948 12.191 9.524 23.542 20.004 27.569 35.346 0.18594 1.3314 1.2377 3.107 0.91673 4.1782-1.351 0.49167-2.7021 0.98335-4.0531 1.475z"/>
      <path id="path2854-1-4" stroke= "black" stroke-width="3.1733" fill="navajowhite" d="m319.32 423.28c15.052-4.6352 30.32-6.0558 45.917-6.8826-1.7163-2.9573-3.4327-5.9145-5.149-8.8718 3.6999 0.98237 7.3999 1.9648 11.1 2.9472 14.465-11.69 28.93-23.38 43.395-35.069l-56.862 3.3513-5.9044 12.816c-2.2532-1.3279-5.3646-4.1344-7.6178-5.4622-11.876 8.3151-23.014 17.566-27.537 31.727-0.23938 1.2369-1.3109 2.847-1.063 3.8653 1.2403 0.52647 2.4806 1.0529 3.7208 1.5794z"/>
    </g>
  </g>
</svg>"""

let evalCastle(place: PlacementType, scale: Dims): string =
        """
    <svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://creativecommons.org/ns#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="1052.3622"
   height="744.09448"
   id="svg2"
   version="1.1"
   inkscape:version="0.48.4 r9939"
   sodipodi:docname="New document 1"
   viewBox ="0 0 """ + (2*(scale.w*scale.h)|>string) + """ 3000"
  >
  <g
     inkscape:label="Layer 1"
     inkscape:groupmode="layer"
     id="layer1"
     transform="translate(0,-308.2677)">
    <path
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none"
       d="M 264.8125 316.5625 C 263.99296 316.5625 263.3125 317.24296 263.3125 318.0625 L 263.3125 587.28125 C 263.3125 588.10079 263.99296 588.78125 264.8125 588.78125 L 457.125 588.78125 L 457.125 504.71875 C 457.125 504.1701 457.68148 504.07616 458.09375 503.71875 C 479.63392 485.0448 500.2468 485.04476 519.9375 503.71875 C 520.3336 504.09439 520.9375 504.1701 520.9375 504.71875 L 520.9375 588.78125 L 755.9375 588.78125 C 756.75704 588.78125 757.40625 588.10079 757.40625 587.28125 L 757.40625 318.0625 C 757.40625 317.24296 756.75704 316.5625 755.9375 316.5625 L 264.8125 316.5625 z "
       transform="translate(0,308.2677)"
       id="rect2985" />
    <path
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:#ffffff;stroke-width:1.70000017;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0"
       d="M 189.75 279.65625 C 188.46215 279.65625 187.4375 280.6809 187.4375 281.96875 L 187.4375 323.34375 C 187.4375 324.6316 188.46215 325.6875 189.75 325.6875 L 811.28125 325.6875 C 812.5691 325.6875 813.625 324.6316 813.625 323.34375 L 813.625 281.96875 C 813.625 280.6809 812.5691 279.65625 811.28125 279.65625 L 723.84375 279.65625 L 723.84375 301.09375 C 723.84375 301.38491 723.63491 301.625 723.34375 301.625 L 706.59375 301.625 C 706.30259 301.625 706.0625 301.38491 706.0625 301.09375 L 706.0625 279.65625 L 680.96875 279.65625 L 680.96875 303.1875 C 680.96875 303.47866 680.72866 303.71875 680.4375 303.71875 L 663.71875 303.71875 C 663.42759 303.71875 663.1875 303.47866 663.1875 303.1875 L 663.1875 279.65625 L 629.71875 279.65625 L 629.71875 301.09375 C 629.71875 301.38491 629.47866 301.625 629.1875 301.625 L 612.46875 301.625 C 612.17759 301.625 611.9375 301.38491 611.9375 301.09375 L 611.9375 279.65625 L 577.40625 279.65625 L 577.40625 300.0625 C 577.40625 300.35366 577.16616 300.5625 576.875 300.5625 L 560.15625 300.5625 C 559.86509 300.5625 559.625 300.35366 559.625 300.0625 L 559.625 279.65625 L 527.1875 279.65625 L 527.1875 296.90625 C 527.1875 297.19741 526.97866 297.4375 526.6875 297.4375 L 509.9375 297.4375 C 509.64634 297.4375 509.40625 297.19741 509.40625 296.90625 L 509.40625 279.65625 L 473.84375 279.65625 L 473.84375 299 C 473.84375 299.29116 473.60366 299.53125 473.3125 299.53125 L 456.59375 299.53125 C 456.30259 299.53125 456.0625 299.29116 456.0625 299 L 456.0625 279.65625 L 413.1875 279.65625 L 413.1875 297.96875 C 413.1875 298.25991 412.94741 298.5 412.65625 298.5 L 395.9375 298.5 C 395.64634 298.5 395.40625 298.25991 395.40625 297.96875 L 395.40625 279.65625 L 356.6875 279.65625 L 356.6875 299 C 356.6875 299.29116 356.44741 299.53125 356.15625 299.53125 L 339.4375 299.53125 C 339.14634 299.53125 338.90625 299.29116 338.90625 299 L 338.90625 279.65625 L 303.34375 279.65625 L 303.34375 299 C 303.34375 299.29116 303.10366 299.53125 302.8125 299.53125 L 286.09375 299.53125 C 285.80259 299.53125 285.5625 299.29116 285.5625 299 L 285.5625 279.65625 L 189.75 279.65625 z "
       transform="translate(0,308.2677)"
       id="rect3767" />
    <path
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:#ffffff;stroke-width:4.19999981;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none"
       d="m 169.21837,571.58542 88.45791,0 18.95526,328.4075 -124.26229,0 z"
       id="rect2987"
       inkscape:connector-curvature="0"
       sodipodi:nodetypes="ccccc" />
    <path
       sodipodi:nodetypes="ccccc"
       inkscape:connector-curvature="0"
       id="path3761"
       d="m 747.62979,571.58542 88.45791,0 18.95526,328.4075 -124.26229,0 z"
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:#ffffff;stroke-width:4.19999981;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none" />
    <path
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:#ffffff;stroke-width:1.70000017;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0"
       d="M 171.96875 215.84375 C 170.6809 215.84375 169.65625 216.89965 169.65625 218.1875 L 169.65625 259.5625 C 169.65625 260.85035 170.6809 261.875 171.96875 261.875 L 252.71875 261.875 C 254.0066 261.875 255.03125 260.85035 255.03125 259.5625 L 255.03125 218.1875 C 255.03125 216.89965 254.0066 215.84375 252.71875 215.84375 L 240.59375 215.84375 L 240.59375 227.875 C 240.59375 228.16616 240.35366 228.40625 240.0625 228.40625 L 225.4375 228.40625 C 225.14634 228.40625 224.90625 228.16616 224.90625 227.875 L 224.90625 215.84375 L 202.9375 215.84375 L 202.9375 226.84375 C 202.9375 227.13491 202.69741 227.34375 202.40625 227.34375 L 187.75 227.34375 C 187.45884 227.34375 187.25 227.13491 187.25 226.84375 L 187.25 215.84375 L 171.96875 215.84375 z "
       transform="translate(0,308.2677)"
       id="rect3769" />
    <path
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:#ffffff;stroke-width:1.70000017;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0"
       d="M 750.4375 215.84375 C 749.14965 215.84375 748.09375 216.89965 748.09375 218.1875 L 748.09375 259.5625 C 748.09375 260.85035 749.14965 261.875 750.4375 261.875 L 831.15625 261.875 C 832.4441 261.875 833.5 260.85035 833.5 259.5625 L 833.5 218.1875 C 833.5 216.89965 832.4441 215.84375 831.15625 215.84375 L 818 215.84375 L 818 228.90625 C 818 229.19741 817.75991 229.4375 817.46875 229.4375 L 802.84375 229.4375 C 802.55259 229.4375 802.3125 229.19741 802.3125 228.90625 L 802.3125 215.84375 L 778.25 215.84375 L 778.25 227.875 C 778.25 228.16616 778.00991 228.40625 777.71875 228.40625 L 763.09375 228.40625 C 762.80259 228.40625 762.5625 228.16616 762.5625 227.875 L 762.5625 215.84375 L 750.4375 215.84375 z "
       transform="translate(0,308.2677)"
       id="rect3771" />
    <rect
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none;stroke-width:1.70000005000000010;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0"
       id="rect3820"
       width="3.8114154"
       height="106.91084"
       x="488.13776"
       y="491.52744"
       ry="0.26857483" />
    <path
       style="fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none"
       d="m 453.85648,504.23663 32.09491,-10.95249 c 0.10867,-0.0371 0.39176,-0.0524 0.39209,0 l 0.13411,21.85918 c 3.3e-4,0.0524 0.35831,0.0116 0.24971,0.0486 l -0.38388,0.131 c -0.10866,0.0371 -0.28348,0.0371 -0.39208,0 l -32.09492,-10.9525 c -0.10866,-0.0371 -0.10866,-0.0967 0,-0.13379 z"
       id="rect3827"
       inkscape:connector-curvature="0"
       sodipodi:nodetypes="sssssssss" />
  </g>
</svg>"""

let evalName(name: string, outer_scale: Dims)(env: Map<string, Dims * string>): string =
    let mod_name = ("Name \"" + name + "\"")
    if Map.containsKey mod_name env then
        let inner_scale, svg = env[mod_name]
        // TODO: CHANGE THE SCALE HERE
        svg
        else
            printfn "Undefined variable."
            exit 1

let evalCircle (point,radius, scale: Dims): string =
    let mininimum = min scale.w scale.h
    let width = scale.w/2
    let height = scale.h/2
    "   <circle cx =\"" +  (((point.x)+width+height) |> string) + "\"" +
    " cy =\"" +  ((point.y+width+height) |> string) + "\"" +
    " r =\"" + ((radius*(mininimum/5)) |> string) + "\"" +
    " stroke= \"black\" stroke-width=\"4\" fill =\"" + canvas_color + "\"/>\n"