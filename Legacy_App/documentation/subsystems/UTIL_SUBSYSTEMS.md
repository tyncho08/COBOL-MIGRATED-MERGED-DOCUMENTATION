# UTILITY SUBSYSTEMS - Date, Currency & Integration Services

## DATE_UTIL - Date Utilities Subsystem

### Especificaciones
Proporciona funciones centralizadas para manejo de fechas, cálculos temporales y conversiones de formato en todo el sistema ACAS.

#### Componentes Principales
- Conversión entre formatos de fecha COBOL y SQL
- Cálculo de días laborables y períodos contables
- Validación de fechas y rangos temporales
- Manejo de años fiscales y períodos de reporte

#### Funciones Principales
```cobol
*> Conversión de formatos
01 DATE-FUNCTIONS.
   03 COBOL-TO-SQL-DATE     PIC X(10).  *> YYYY-MM-DD
   03 SQL-TO-COBOL-DATE     PIC 9(8).   *> YYYYMMDD
   03 DISPLAY-FORMAT-DATE   PIC X(12).  *> MM/DD/YYYY

*> Cálculos temporales
01 DATE-CALCULATIONS.
   03 DAYS-BETWEEN-DATES    PIC S9(5).
   03 BUSINESS-DAYS-COUNT   PIC S9(4).
   03 FISCAL-PERIOD         PIC 99.
   03 FISCAL-YEAR           PIC 9999.

*> Validaciones
01 DATE-VALIDATIONS.
   03 DATE-IS-VALID         PIC X.      *> Y/N
   03 DATE-IN-RANGE         PIC X.      *> Y/N
   03 DATE-IS-FUTURE        PIC X.      *> Y/N
```

#### Servicios Ofrecidos
- **Conversión**: Entre formatos COBOL (PIC 9(8)) y SQL (DATE)
- **Validación**: Verificación de fechas válidas y rangos
- **Cálculo**: Diferencias entre fechas, días laborables
- **Períodos**: Determinación de períodos fiscales y contables
- **Formateo**: Presentación en múltiples formatos de salida

---

## CURR_UTIL - Currency Utilities Subsystem

### Especificaciones
Maneja conversiones monetarias, formateo de moneda y cálculos financieros con precisión decimal requerida para aplicaciones contables.

#### Componentes Principales
- Conversión entre tipos de moneda
- Formateo para presentación e impresión
- Cálculos con redondeo contable
- Validación de importes y rangos

#### Estructuras de Datos
```cobol
*> Definición de moneda
01 CURRENCY-DEFINITION.
   03 CURRENCY-CODE         PIC XXX.    *> USD, EUR, etc.
   03 CURRENCY-SYMBOL       PIC X(3).   *> $, €, £
   03 DECIMAL-PLACES        PIC 9.      *> 2 para USD
   03 THOUSANDS-SEPARATOR   PIC X.      *> ,
   03 DECIMAL-SEPARATOR     PIC X.      *> .

*> Valores monetarios
01 CURRENCY-VALUES.
   03 AMOUNT-INTERNAL       PIC S9(11)V99 COMP-3.
   03 AMOUNT-DISPLAY        PIC $,$$$,$$$,$$9.99.
   03 AMOUNT-EDIT           PIC Z(9).99-.
   03 EXCHANGE-RATE         PIC 9(3)V9(6).
```

#### Funciones Principales
- **Formateo**: Conversión a formato de presentación con símbolos
- **Redondeo**: Aplicación de reglas de redondeo contable
- **Conversión**: Entre diferentes denominaciones monetarias
- **Validación**: Verificación de rangos y formatos válidos
- **Cálculos**: Operaciones financieras con precisión requerida

---

## INTEGRATION - Integration Services Subsystem

### Especificaciones
Proporciona servicios de integración con sistemas externos, incluyendo importación/exportación de datos, interfaces de comunicación y servicios de sincronización.

#### Componentes Principales

1. **Data Import/Export Services**
   - Importación desde archivos CSV, XML, JSON
   - Exportación a formatos estándar
   - Mapeo de campos automático
   - Validación de datos importados

2. **Communication Interfaces**
   - send-some-mail.cbl - Servicio de email
   - send-mail-test-example.cbl - Testing de email
   - API REST interfaces (módulos futuros)
   - File transfer protocols

3. **System Integration**
   - makesqltable.cbl - Generación de tablas SQL
   - Sincronización con sistemas ERP externos
   - Interfaces con servicios web
   - Message queuing systems

#### Interfaces de Integración
```cobol
*> Configuración de integración
01 INTEGRATION-CONFIG.
   03 INTERFACE-TYPE        PIC XX.     *> EM/FT/WS/API
   03 ENDPOINT-URL          PIC X(255).
   03 AUTH-METHOD           PIC XX.     *> BA/TK/CT
   03 CREDENTIALS           PIC X(100).
   03 TIMEOUT-SECONDS       PIC 999.
   03 RETRY-COUNT           PIC 9.
   03 BATCH-SIZE            PIC 9999.

*> Resultado de operación
01 INTEGRATION-RESULT.
   03 OPERATION-STATUS      PIC XX.     *> OK/ER/WN
   03 RECORDS-PROCESSED     PIC 9(8).
   03 RECORDS-FAILED        PIC 9(6).
   03 ERROR-MESSAGE         PIC X(255).
   03 RESPONSE-TIME         PIC 9(4).
```

#### Servicios de Email
```cobol
*> Configuración de email
01 EMAIL-CONFIG.
   03 SMTP-SERVER           PIC X(50).
   03 SMTP-PORT             PIC 9999.
   03 USE-SSL               PIC X.      *> Y/N
   03 USERNAME              PIC X(30).
   03 PASSWORD              PIC X(30).

*> Mensaje de email
01 EMAIL-MESSAGE.
   03 FROM-ADDRESS          PIC X(60).
   03 TO-ADDRESS            PIC X(300). *> Multiple recipients
   03 CC-ADDRESS            PIC X(300).
   03 BCC-ADDRESS           PIC X(300).
   03 SUBJECT               PIC X(100).
   03 BODY-TEXT             PIC X(2000).
   03 ATTACHMENT-PATH       PIC X(255).
   03 HTML-FORMAT           PIC X.      *> Y/N
```

## Flujos de Proceso de Utilidades

### Flujo de Procesamiento de Fechas
1. **Entrada**: Recibir fecha en formato origen
2. **Validación**: Verificar validez y rango permitido
3. **Conversión**: Transformar al formato destino
4. **Cálculo**: Aplicar operaciones temporales si es necesario
5. **Salida**: Retornar fecha procesada y códigos de estado

### Flujo de Procesamiento Monetario
1. **Entrada**: Recibir valor monetario
2. **Validación**: Verificar rango y formato
3. **Conversión**: Aplicar formato y redondeo
4. **Cálculo**: Ejecutar operaciones financieras
5. **Presentación**: Formatear para salida específica

### Flujo de Integración
1. **Configuración**: Leer parámetros de conexión
2. **Conexión**: Establecer enlace con sistema externo
3. **Autenticación**: Validar credenciales
4. **Transferencia**: Ejecutar operación de datos
5. **Validación**: Verificar integridad de datos
6. **Confirmación**: Registrar resultado de operación

## Dependencias Comunes

### Archivos de Sistema
- system.dat: Configuración global
- utility.params: Parámetros específicos de utilidades
- format.config: Configuración de formatos

### Módulos Compartidos
- Error handling framework
- Logging system (fhlogger)
- Configuration management
- System output (ACAS-Sysout)

## Manejo de Errores de Utilidades

### Códigos de Error de Fecha (DT001-DT999)
- DT001: Fecha inválida
- DT002: Formato de fecha no soportado
- DT003: Fecha fuera de rango
- DT004: Error en cálculo temporal

### Códigos de Error de Moneda (CU001-CU999)
- CU001: Importe fuera de rango
- CU002: Formato de moneda inválido
- CU003: Error en conversión monetaria
- CU004: Código de moneda no reconocido

### Códigos de Error de Integración (IN001-IN999)
- IN001: Error de conexión
- IN002: Autenticación fallida
- IN003: Formato de datos incompatible
- IN004: Timeout en operación
- IN005: Error en servicio externo

## Características Especiales

### Localización
- Soporte para múltiples formatos regionales
- Configuración de símbolos monetarios por región
- Formatos de fecha internacionales
- Mensajes de error localizados

### Performance Optimizada
- Cache de configuraciones frecuentes
- Algoritmos optimizados para cálculos
- Reutilización de conexiones
- Procesamiento en batch para operaciones masivas

### Configuración Flexible
- Parámetros configurables sin recompilación
- Plantillas de formato personalizables
- Reglas de negocio configurables
- Interfaces adaptables a diferentes sistemas

## Notas de Implementación

### Consideraciones de Diseño
- APIs consistentes entre utilidades
- Manejo de errores estandarizado
- Documentación completa de funciones
- Testing automatizado extensivo

### Limitaciones
- Formatos de fecha limitados a estándares comunes
- Soporte monetario para monedas principales
- Integración limitada a protocolos estándar
- Dependencia de configuración externa

### Mejores Prácticas
- Validar todas las entradas
- Usar constantes para códigos de formato
- Implementar logging detallado
- Manejar excepciones gracefully
- Documentar cambios de configuración
- Probar con datos de producción realistas