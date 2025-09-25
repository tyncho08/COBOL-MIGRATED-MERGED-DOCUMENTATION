# ACAS System - Subsystems Architecture Overview

## Resumen Ejecutivo

El sistema ACAS está compuesto por 14 subsistemas principales que proporcionan una arquitectura modular y robusta para la gestión contable y financiera. Esta documentación completa cubre las especificaciones, interfaces, flujos de proceso y dependencias de cada subsistema crítico.

## Subsistemas Documentados

### 1. BATCH_FW - Batch Framework
**Propósito**: Framework de procesamiento batch para carga/descarga de datos  
**Componentes**: glbatchLD.cbl, glbatchUNL.cbl, glbatchMT.cbl, glbatchRES.cbl  
**Criticidad**: ALTA - Central para operaciones de datos masivos  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- Carga de datos desde archivos COBOL a tablas MySQL
- Procesamiento transaccional con commit/rollback
- Manejo de duplicados con lógica de reescritura
- Backup y restauración de datos batch

---

### 2. FILE_SVC - File Services
**Propósito**: Servicios centralizados de manejo de archivos y acceso a datos  
**Componentes**: Patrón LD/UNL/RES/MT para todas las entidades  
**Criticidad**: CRÍTICA - Base para todas las operaciones de datos  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- Abstracción unificada para archivos COBOL e RDB
- Operaciones CRUD estándar (Open, Read, Write, Delete)
- Modo dual (archivo + RDB simultáneo)
- Conversión automática entre formatos

---

### 3. MDM - Master Data Management
**Propósito**: Gestión centralizada de datos maestros críticos  
**Componentes**: nominal, system, value, analysis  
**Criticidad**: CRÍTICA - Fundamento de todas las operaciones  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- Chart of Accounts (COA) jerárquico
- Configuración del sistema centralizada
- Parámetros de valores tipados
- Códigos de análisis y departamento

---

### 4. SEC_AUDIT - Security and Audit
**Propósito**: Sistema de auditoría y seguridad completo  
**Componentes**: auditLD.cbl, auditMT.cbl, auditUNL.cbl, auditRES.cbl  
**Criticidad**: ALTA - Cumplimiento regulatorio  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- Rastro de auditoría completo de transacciones
- Registro de movimientos de inventario
- Control de acceso y permisos
- Integridad de datos transaccional

---

### 5. RPT_ENGINE - Report Engine
**Propósito**: Motor de generación de reportes financieros y operacionales  
**Componentes**: gl0xx.cbl, sl0xx.cbl, pl0xx.cbl, st0xx.cbl, irs0xx.cbl  
**Criticidad**: ALTA - Reporte gerencial y regulatorio  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- Reportes financieros (Balance Sheet, P&L)
- Análisis de ventas y compras
- Reportes de inventario y stock
- Formularios regulatorios (IRS, Tax)

---

### 6. ERROR_FW - Error Framework
**Propósito**: Framework centralizado de manejo de errores y logging  
**Componentes**: fhlogger.cbl, ACAS-Sysout.cbl, códigos estandarizados  
**Criticidad**: ALTA - Estabilidad y diagnóstico del sistema  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- Códigos de error estandarizados por módulo
- Logging estructurado y rotación automática
- Recuperación automática de errores
- Notificaciones y alertas escalables

---

### 7-9. UTILITY SUBSYSTEMS
**Propósito**: Servicios de utilidad especializados  
**Componentes**: DATE_UTIL, CURR_UTIL, INTEGRATION  
**Criticidad**: MEDIA - Soporte a operaciones principales  
**Estado de Documentación**: ✅ COMPLETA

**Funcionalidades Clave**:
- **DATE_UTIL**: Manejo de fechas y períodos fiscales
- **CURR_UTIL**: Formateo y cálculos monetarios
- **INTEGRATION**: Interfaces con sistemas externos

---

### 10-14. SUBSISTEMAS ADICIONALES IDENTIFICADOS

#### 10. TRANS_ENGINE - Transaction Engine
**Propósito**: Motor de procesamiento transaccional  
**Componentes**: posting*.cbl, payment*.cbl, invoice*.cbl  
**Estado**: 🔄 IDENTIFICADO - Documentación pendiente

#### 11. USER_MGT - User Management
**Propósito**: Gestión de usuarios y sesiones  
**Componentes**: Módulos de autenticación y autorización  
**Estado**: 🔄 IDENTIFICADO - Documentación pendiente

#### 12. INTERFACE_MGT - Interface Management
**Propósito**: Gestión de interfaces de usuario  
**Componentes**: Pantallas, menús, navegación  
**Estado**: 🔄 IDENTIFICADO - Documentación pendiente

#### 13. BACKUP_SVC - Backup Services
**Propósito**: Servicios de backup y recuperación  
**Componentes**: UNL/RES pattern modules  
**Estado**: 🔄 IDENTIFICADO - Documentación pendiente

#### 14. CONFIG_MGT - Configuration Management
**Propósito**: Gestión centralizada de configuración  
**Componentes**: Archivos .param, variables de entorno  
**Estado**: 🔄 IDENTIFICADO - Documentación pendiente

## Arquitectura de Interconexión

### Dependencias Críticas
```
[BATCH_FW] --> [FILE_SVC] --> [MDM]
     |              |           |
     v              v           v
[SEC_AUDIT] <-- [ERROR_FW] --> [RPT_ENGINE]
     |              |           |
     v              v           v
[DATE_UTIL] <-- [CURR_UTIL] --> [INTEGRATION]
```

### Flujos de Datos Principales
1. **Entrada de Datos**: INTEGRATION → FILE_SVC → MDM
2. **Procesamiento**: BATCH_FW → TRANS_ENGINE → SEC_AUDIT
3. **Salida/Reporte**: RPT_ENGINE → FILE_SVC → INTEGRATION
4. **Soporte**: ERROR_FW, UTIL_SUBSYSTEMS → Todos los módulos

## Información Crítica No Documentada Previamente

### 1. Arquitectura de Datos Dual
- **Descubrimiento**: Sistema soporta operación simultánea con archivos COBOL ISAM y tablas relacionales MySQL
- **Implicación**: Permite migración gradual y redundancia de datos
- **Criticidad**: ALTA para estrategias de modernización

### 2. Framework de Manejo de Errores Estandarizado
- **Descubrimiento**: Códigos de error consistentes y estructurados por subsistema
- **Componentes**: Más de 100 códigos específicos catalogados
- **Implicación**: Facilita diagnóstico y mantenimiento del sistema

### 3. Sistema de Logging y Auditoría Completo
- **Descubrimiento**: Rastro completo de todas las operaciones con fhlogger
- **Capacidades**: Logging estructurado, rotación automática, múltiples niveles
- **Implicación**: Cumplimiento regulatorio y debugging avanzado

### 4. Arquitectura de Conversión de Datos Automatizada
- **Descubrimiento**: Conversión transparente entre formatos COBOL y SQL
- **Implementación**: Host Variables (HV) y mapeo automático
- **Implicación**: Reduce complejidad de desarrollo y mantenimiento

### 5. Sistema de Recuperación Transaccional
- **Descubrimiento**: Soporte para commit/rollback con MySQL
- **Requisito**: MySQL debe configurarse con autocommit=OFF
- **Implicación**: Integridad transaccional completa

### 6. Framework de Reportes Extensible
- **Descubrimiento**: Más de 50 reportes especializados identificados
- **Arquitectura**: Patrón consistente con parámetros configurables
- **Implicación**: Fácil extensión para nuevos reportes

## Recomendaciones Estratégicas

### Prioridades de Modernización
1. **CRÍTICO**: Mantener compatibilidad con FILE_SVC y MDM
2. **ALTO**: Migrar ERROR_FW a estándares modernos de logging
3. **ALTO**: Evolucionar RPT_ENGINE hacia herramientas de BI
4. **MEDIO**: Modernizar INTEGRATION para APIs REST

### Documentación Pendiente
1. **TRANS_ENGINE**: Mapear todos los módulos de procesamiento transaccional
2. **USER_MGT**: Documentar sistema de autenticación y autorización
3. **INTERFACE_MGT**: Catalogar pantallas y flujos de usuario
4. **CONFIG_MGT**: Documentar todos los parámetros de configuración

### Consideraciones de Migración
1. **Datos**: Utilizar modo dual para migración sin interrupción
2. **Interfaces**: Preservar APIs existentes durante transición
3. **Reportes**: Mantener compatibilidad con reportes críticos
4. **Auditoría**: Preservar rastros de auditoría durante migración

## Estado del Proyecto

**Subsistemas Completamente Documentados**: 9 de 14 (64%)  
**Información Crítica Capturada**: ✅ COMPLETA  
**Análisis de Dependencias**: ✅ COMPLETA  
**Recomendaciones Estratégicas**: ✅ COMPLETA  

**Próximos Pasos**:
1. Completar documentación de subsistemas restantes
2. Validar información con stakeholders técnicos
3. Crear plan de modernización detallado
4. Establecer métricas de migración

---

*Documentación generada el 25 de septiembre de 2025*  
*Sistema ACAS - Versión 3.02 y posteriores*  
*Arquitectura COBOL legacy con capacidades de modernización*