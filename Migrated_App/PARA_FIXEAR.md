# PLAN MAESTRO DE CORRECCIÓN - APLICACIÓN MIGRADA COBOL

## 🔍 DIAGNÓSTICO SISTÉMICO CRÍTICO

### ESTADO ACTUAL DE LOS MÓDULOS

| Módulo   | Estado      | Problemas Críticos                   | Nivel de Riesgo |
|----------|-------------|--------------------------------------|-----------------|
| PAYMENTS | ✅ FUNCIONAL | Sistema operativo                    | 🟢 FUNCIONAL    |
| SALES    | ✅ FUNCIONAL | Endpoints creados y funcionando      | 🟢 FUNCIONAL    |
| PURCHASE | 🔴 ROTO     | Modelos faltantes, imports rotos      | 🔴 NO FUNCIONAL |
| STOCK    | 🟡 PARCIAL  | Modelos creados, servicios rotos      | 🟡 LIMITADO     |
| GL       | 🔴 ROTO     | Conflictos de modelos, imports rotos  | 🔴 NO FUNCIONAL |
| REPORTS  | 🔴 ROTO     | Dependencias faltantes               | 🔴 NO FUNCIONAL |

---

## 🚨 PROBLEMAS CRÍTICOS IDENTIFICADOS

### 1. ERRORES DE STARTUP FATAL
```
ModuleNotFoundError: No module named 'app.database'
```
**Ubicación:** `app/api/v1/admin/settings.py:5`
**Causa:** Import incorrecto - debería ser `from app.core.database import get_db`

### 2. MODELOS FALTANTES CRÍTICOS

#### PURCHASE Module - Modelos No Implementados:
- `PurchaseOpenItemRec` - Requerido por servicios
- `PurchaseHistoryRec` - Requerido por reportes
- `PurchaseOrderRec` - Requerido por órdenes de compra
- `SupplierLedgerRec` - Requerido por contabilidad

#### WAREHOUSE Module - Modelos Incompletos:
- `WarehouseRec` - Definición básica faltante
- Relaciones con Stock no establecidas

### 3. CONFLICTO DE MODELOS GL
**Problema:** Existe `GLAccount` pero servicios buscan `GLLedgerRec`
**Archivos afectados:**
- `models/gl.py` - Define GLAccount
- `services/gl/*` - Esperan GLLedgerRec

### 4. IMPORTS CIRCULARES/ROTOS
- Rutas de imports inconsistentes
- Dependencias circulares entre módulos
- Paths absolutos vs relativos mezclados

### 5. INCONSISTENCIAS FRONTEND-BACKEND
- Frontend espera datos que backend no proporciona
- Schemas no alineados con modelos de base de datos
- APIs que retornan estructuras diferentes

### 6. ⚠️ DATOS HARDCODEADOS/MOCK EN FRONTEND (CRÍTICO URGENTE)
**PROBLEMA CRÍTICO:** Frontend puede contener datos simulados que no provienen de la base de datos
**IMPACTO:** Sistema no refleja datos reales, reportes incorrectos, inconsistencias operativas
**REQUERIMIENTO:** Auditoría milimétrica de cada dato mostrado en frontend

---

## 🎯 PLAN MAESTRO DE CORRECCIÓN

### FASE 0: AUDITORÍA CRÍTICA FRONTEND-DATABASE (URGENTE - Días 1-3)
**Objetivo:** Verificar que ABSOLUTAMENTE TODOS los datos en frontend provienen de la base de datos

#### 0.1 Auditoría Milimétrica Frontend
- [ ] **CUSTOMERS Page:** Verificar cada campo, tabla, gráfico viene de API/DB
- [ ] **SALES Page:** Validar que todos los datos de ventas son reales
- [ ] **PURCHASE Page:** Confirmar que datos de compras no son mock
- [ ] **STOCK Page:** Verificar inventario refleja DB real
- [ ] **GL Page:** Validar que contabilidad viene de DB
- [ ] **PAYMENTS Page:** Confirmar transacciones son reales
- [ ] **REPORTS Page:** Verificar que reportes usan datos reales
- [ ] **SETTINGS Page:** Confirmar configuraciones vienen de DB

#### 0.2 Mapeo Datos Frontend → API → Database
- [ ] Crear matriz completa: `Componente → API Endpoint → Database Table → Field`
- [ ] Identificar TODOS los datos hardcodeados/simulados
- [ ] Documentar APIs faltantes para datos mostrados
- [ ] Marcar componentes que usan datos mock

#### 0.3 Validación de Integridad de Datos
- [ ] Cross-reference: ¿Cada número en pantalla tiene origen en DB?
- [ ] Verificar cálculos: ¿Totales, sumas, promedios son calculados o hardcoded?
- [ ] Validar fechas: ¿Timestamps reales o valores fijos?
- [ ] Confirmar IDs: ¿Referencias a registros reales o datos inventados?

#### 0.4 Crear Inventario Completo de Gaps
- [ ] Lista de componentes que usan datos mock
- [ ] APIs que faltan implementar
- [ ] Endpoints que retornan datos incorrectos
- [ ] Campos de frontend sin equivalente en DB

### FASE 1: FUNDAMENTOS CRÍTICOS (Semana 1)
**Objetivo:** Hacer que la aplicación arranque sin errores

#### 1.1 Reparar Imports Críticos
- [ ] Corregir `app.database` → `app.core.database` en todos los archivos
- [ ] Estandarizar todas las rutas de import
- [ ] Eliminar imports circulares

#### 1.2 Resolver Conflicto GL
- [ ] **DECISIÓN CRÍTICA:** Elegir entre GLAccount vs GLLedgerRec
- [ ] Actualizar todos los servicios para usar el modelo elegido
- [ ] Crear migrations si es necesario

#### 1.3 Crear Modelos Faltantes Críticos
- [ ] Implementar `PurchaseOpenItemRec`
- [ ] Implementar `PurchaseHistoryRec` 
- [ ] Implementar `PurchaseOrderRec`
- [ ] Completar `WarehouseRec` con relaciones

### FASE 2: SERVICIOS Y FUNCIONALIDAD (Semana 2-3)
**Objetivo:** Hacer que los módulos funcionen básicamente

#### 2.1 Módulo PURCHASE
- [ ] Implementar métodos faltantes en servicios
- [ ] Crear APIs básicas para órdenes de compra
- [ ] Establecer flujo básico purchase → GL

#### 2.2 Módulo STOCK
- [ ] Corregir servicios que usan modelos obsoletos
- [ ] Implementar integración con Warehouse
- [ ] Reparar flujos de movimientos de stock

#### 2.3 Módulo GL Integration
- [ ] Crear núcleo de integración GL que todos los módulos necesitan
- [ ] Implementar posting automático
- [ ] Establecer conexiones con AP/AR

#### 2.4 Reparar Queries SQL
- [ ] Actualizar field names en servicios
- [ ] Alinear Pydantic schemas con SQLAlchemy models
- [ ] Corregir table names inconsistentes

### FASE 3: INTEGRACIÓN Y TESTING (Semana 4)
**Objetivo:** Asegurar que todo funciona end-to-end

#### 3.1 Testing End-to-End
- [ ] Probar cada módulo individualmente
- [ ] Verificar flujos inter-módulos
- [ ] Validar integración frontend-backend

#### 3.2 Alineación Frontend-Backend
- [ ] Verificar que APIs retornan datos esperados por frontend
- [ ] Corregir inconsistencias en schemas
- [ ] Implementar endpoints faltantes

#### 3.3 Business Logic y Validaciones
- [ ] Implementar validaciones de business rules
- [ ] Corregir cálculos financieros
- [ ] Establecer controles de integridad

### FASE 4: OPTIMIZACIÓN Y PULIDO (Semana 5)
**Objetivo:** Mejorar performance y estabilidad

#### 4.1 Performance
- [ ] Optimizar queries lentas
- [ ] Implementar caching donde sea necesario
- [ ] Mejorar índices de base de datos

#### 4.2 Estabilidad
- [ ] Implementar manejo de errores robusto
- [ ] Agregar logging comprehensivo
- [ ] Establecer monitoring básico

---

## 🛠️ ESTRATEGIA DE IMPLEMENTACIÓN

### Enfoque Recomendado: "Fix and Test"
1. **Atacar por criticidad:** Startup errors → Modelos → Servicios → Integration
2. **Probar incrementalmente:** Después de cada fix, verificar que no se rompe nada más
3. **Mantener funcionalidad existente:** SALES y PAYMENTS ya funcionan - no tocar

### Prioridades por Orden:
1. **URGENTE CRÍTICO:** Auditoría completa frontend-database (3 días)
2. **CRÍTICO:** Resolver startup errors (imports rotos)
3. **ALTO:** Crear modelos faltantes (PURCHASE principalmente)
4. **ALTO:** Resolver conflicto GL
5. **MEDIO:** Reparar servicios STOCK
6. **MEDIO:** Implementar GL Integration
7. **BAJO:** Optimizaciones y pulido

---

## 📋 CHECKLIST DE VALIDACIÓN

### Pre-Implementación
- [ ] **URGENTE:** Completar auditoría frontend-database (FASE 0)
- [ ] Crear inventario completo de datos mock vs reales
- [ ] Backup completo de código actual
- [ ] Documentar estado actual de cada módulo
- [ ] Preparar entorno de testing

### Durante Implementación  
- [ ] Cada fix debe pasar tests básicos
- [ ] Mantener log de cambios realizados
- [ ] Verificar que módulos funcionando no se rompan

### Post-Implementación
- [ ] Testing end-to-end completo
- [ ] Validación con datos reales
- [ ] Performance testing básico
- [ ] Documentación de cambios

---

## ⚠️ RIESGOS Y CONSIDERACIONES

### Riesgos Altos
- **Romper funcionalidad existente:** SALES y PAYMENTS ya funcionan
- **Crear nuevas dependencias circulares** al arreglar imports
- **Incompatibilidad de datos** al cambiar modelos GL

### Mitigaciones
- **Testing continuo** después de cada cambio
- **Rollback plan** para cada fase
- **Backup de datos** antes de migrations

### Consideraciones Técnicas
- **Database migrations** pueden ser necesarias
- **Frontend updates** para alinear con backend changes
- **API versioning** si cambian contratos

---

## 🎯 MÉTRICAS DE ÉXITO

### Fase 0 - Auditoría Frontend-Database (CRÍTICO)
- [ ] **100% de datos frontend trackeados a database**
- [ ] Cero datos hardcodeados en componentes
- [ ] Inventario completo de APIs faltantes
- [ ] Matriz completa: Frontend → API → Database

### Fase 1 - Fundamentos
- [ ] Aplicación arranca sin errores
- [ ] Todos los módulos importan correctamente
- [ ] No hay conflictos de modelos

### Fase 2 - Funcionalidad  
- [ ] PURCHASE module funciona básicamente
- [ ] STOCK operations funcionan
- [ ] GL integration operativa

### Fase 3 - Integración
- [ ] Todos los módulos pasan tests end-to-end
- [ ] Frontend-backend alineados
- [ ] Business logic validada

### Fase 4 - Optimización
- [ ] Performance aceptable
- [ ] Manejo de errores robusto
- [ ] Sistema estable para producción

---

## 🔧 HERRAMIENTAS REQUERIDAS

### Para Debugging
- PostgreSQL client para verificar schema
- Postman/curl para testing de APIs
- Python debugger para troubleshooting

### Para Testing
- pytest para unit tests
- Requests para integration tests
- Frontend dev tools para UI testing
- **Browser DevTools para auditoría de datos frontend**
- **Network tab para tracking de API calls**

### Para Monitoring
- Logs estructurados para tracking
- Database monitoring para performance
- API response time monitoring

---

**ESTIMATED TOTAL TIME:** 5-6 semanas (incluyendo auditoría crítica)  
**CRITICAL PATH:** Auditoría frontend-database → Startup errors → PURCHASE models → GL integration  
**SUCCESS METRIC:** **100% datos frontend desde database** + Todos los módulos funcionales y estables para uso en producción

## 🚨 ACCIÓN INMEDIATA REQUERIDA

### DÍAS 1-3: AUDITORÍA MILIMÉTRICA FRONTEND
**OBJETIVO:** Verificar que CADA DATO visible en el frontend proviene de la base de datos

1. **Abrir cada página del frontend**
2. **Inspeccionar cada número, texto, gráfico, tabla**
3. **Trazar origen:** ¿Viene de API call? ¿Es hardcodeado? ¿Es calculado?
4. **Documentar TODO:** Crear lista exhaustiva de datos mock/hardcoded
5. **Priorizar corrección:** APIs críticas que faltan por implementar

**HERRAMIENTAS NECESARIAS:**
- Browser DevTools (Network tab para ver API calls)
- VSCode para revisar componentes de frontend
- PostgreSQL client para verificar data en DB

---

*Este plan debe ejecutarse incrementalmente con validación continua para minimizar riesgos y maximizar estabilidad.*