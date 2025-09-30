'use client'

import { useState, useEffect } from 'react'
import {
  ChartBarIcon,
  TableCellsIcon,
  FunnelIcon,
  ArrowDownTrayIcon,
  PlayIcon,
  SaveIcon,
  FolderOpenIcon,
  TrashIcon,
  PlusIcon,
  XMarkIcon,
  ArrowPathIcon,
  DocumentDuplicateIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import PageHeader from '@/components/Layout/PageHeader'
import Button from '@/components/UI/Button'
import Input from '@/components/UI/Input'
import Select from '@/components/UI/Select'
import Modal from '@/components/UI/Modal'
import Table from '@/components/UI/Table'

interface CustomReport {
  id: string
  name: string
  description: string
  tables: SelectedTable[]
  filters: FilterCondition[]
  groupBy: string[]
  orderBy: OrderByClause[]
  lastModified: string
  createdBy: string
}

interface SelectedTable {
  tableName: string
  alias?: string
  columns: SelectedColumn[]
  joins?: JoinClause[]
}

interface SelectedColumn {
  columnName: string
  displayName?: string
  aggregate?: 'SUM' | 'AVG' | 'COUNT' | 'MIN' | 'MAX'
  format?: 'currency' | 'date' | 'percentage' | 'number'
}

interface JoinClause {
  joinType: 'INNER' | 'LEFT' | 'RIGHT'
  tableName: string
  on: string
}

interface FilterCondition {
  column: string
  operator: 'equals' | 'not_equals' | 'greater_than' | 'less_than' | 'contains' | 'between'
  value: any
  value2?: any // For between operator
}

interface OrderByClause {
  column: string
  direction: 'ASC' | 'DESC'
}

export default function CustomReportsPage() {
  const [savedReports, setSavedReports] = useState<CustomReport[]>([])
  const [showReportBuilder, setShowReportBuilder] = useState(false)
  const [showSaveModal, setShowSaveModal] = useState(false)
  const [reportData, setReportData] = useState<any[]>([])
  const [loading, setLoading] = useState(false)
  const [executingReport, setExecutingReport] = useState(false)
  
  // Report Builder State
  const [reportName, setReportName] = useState('')
  const [reportDescription, setReportDescription] = useState('')
  const [selectedTables, setSelectedTables] = useState<SelectedTable[]>([])
  const [filters, setFilters] = useState<FilterCondition[]>([])
  const [groupBy, setGroupBy] = useState<string[]>([])
  const [orderBy, setOrderBy] = useState<OrderByClause[]>([])
  const [currentReport, setCurrentReport] = useState<CustomReport | null>(null)

  // Available tables and columns (in a real app, fetch from API)
  const availableTables = {
    sales_invoices: {
      displayName: 'Sales Invoices',
      columns: ['invoice_number', 'customer_code', 'invoice_date', 'due_date', 'total_amount', 'status']
    },
    customers: {
      displayName: 'Customers',
      columns: ['customer_code', 'customer_name', 'email', 'phone', 'credit_limit', 'balance']
    },
    stock_items: {
      displayName: 'Stock Items',
      columns: ['item_code', 'description', 'quantity_on_hand', 'unit_cost', 'selling_price', 'reorder_level']
    },
    purchase_orders: {
      displayName: 'Purchase Orders',
      columns: ['po_number', 'supplier_code', 'order_date', 'delivery_date', 'total_amount', 'status']
    },
    gl_transactions: {
      displayName: 'General Ledger',
      columns: ['transaction_date', 'account_code', 'description', 'debit_amount', 'credit_amount', 'reference']
    }
  }

  useEffect(() => {
    fetchSavedReports()
  }, [])

  const fetchSavedReports = async () => {
    setLoading(true)
    try {
      const response = await fetch('http://localhost:8000/api/v1/reports/custom')
      if (response.ok) {
        const data = await response.json()
        setSavedReports(data.reports || getMockSavedReports())
      } else {
        setSavedReports(getMockSavedReports())
      }
    } catch (error) {
      console.error('Failed to fetch saved reports:', error)
      setSavedReports(getMockSavedReports())
    } finally {
      setLoading(false)
    }
  }

  const getMockSavedReports = (): CustomReport[] => [
    {
      id: 'RPT001',
      name: 'Customer Sales Summary',
      description: 'Monthly sales summary by customer',
      tables: [{
        tableName: 'sales_invoices',
        columns: [
          { columnName: 'customer_code', displayName: 'Customer' },
          { columnName: 'total_amount', displayName: 'Total Sales', aggregate: 'SUM', format: 'currency' }
        ]
      }],
      filters: [
        { column: 'invoice_date', operator: 'between', value: '2024-01-01', value2: '2024-12-31' }
      ],
      groupBy: ['customer_code'],
      orderBy: [{ column: 'total_amount', direction: 'DESC' }],
      lastModified: '2024-01-15',
      createdBy: 'admin'
    },
    {
      id: 'RPT002',
      name: 'Low Stock Report',
      description: 'Items below reorder level',
      tables: [{
        tableName: 'stock_items',
        columns: [
          { columnName: 'item_code', displayName: 'Item Code' },
          { columnName: 'description', displayName: 'Description' },
          { columnName: 'quantity_on_hand', displayName: 'Qty on Hand' },
          { columnName: 'reorder_level', displayName: 'Reorder Level' }
        ]
      }],
      filters: [
        { column: 'quantity_on_hand', operator: 'less_than', value: 'reorder_level' }
      ],
      groupBy: [],
      orderBy: [{ column: 'quantity_on_hand', direction: 'ASC' }],
      lastModified: '2024-01-14',
      createdBy: 'admin'
    }
  ]

  const handleNewReport = () => {
    setCurrentReport(null)
    setReportName('')
    setReportDescription('')
    setSelectedTables([])
    setFilters([])
    setGroupBy([])
    setOrderBy([])
    setShowReportBuilder(true)
  }

  const handleEditReport = (report: CustomReport) => {
    setCurrentReport(report)
    setReportName(report.name)
    setReportDescription(report.description)
    setSelectedTables(report.tables)
    setFilters(report.filters)
    setGroupBy(report.groupBy)
    setOrderBy(report.orderBy)
    setShowReportBuilder(true)
  }

  const handleDuplicateReport = (report: CustomReport) => {
    const newReport = { ...report, id: '', name: `${report.name} - Copy` }
    handleEditReport(newReport)
  }

  const handleDeleteReport = async (reportId: string) => {
    if (!confirm('Are you sure you want to delete this report?')) return
    
    try {
      const response = await fetch(`http://localhost:8000/api/v1/reports/custom/${reportId}`, {
        method: 'DELETE'
      })
      if (response.ok) {
        alert('Report deleted successfully')
        fetchSavedReports()
      } else {
        alert('Failed to delete report')
      }
    } catch (error) {
      console.error('Failed to delete report:', error)
      alert('Failed to delete report')
    }
  }

  const handleAddTable = (tableName: string) => {
    if (selectedTables.some(t => t.tableName === tableName)) {
      alert('Table already added')
      return
    }
    
    setSelectedTables([...selectedTables, {
      tableName,
      columns: []
    }])
  }

  const handleAddColumn = (tableIndex: number, columnName: string) => {
    const newTables = [...selectedTables]
    if (!newTables[tableIndex].columns.some(c => c.columnName === columnName)) {
      newTables[tableIndex].columns.push({
        columnName,
        displayName: columnName.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())
      })
      setSelectedTables(newTables)
    }
  }

  const handleRemoveColumn = (tableIndex: number, columnIndex: number) => {
    const newTables = [...selectedTables]
    newTables[tableIndex].columns.splice(columnIndex, 1)
    setSelectedTables(newTables)
  }

  const handleAddFilter = () => {
    setFilters([...filters, {
      column: '',
      operator: 'equals',
      value: ''
    }])
  }

  const handleRemoveFilter = (index: number) => {
    setFilters(filters.filter((_, i) => i !== index))
  }

  const handleAddOrderBy = () => {
    setOrderBy([...orderBy, {
      column: '',
      direction: 'ASC'
    }])
  }

  const handleRemoveOrderBy = (index: number) => {
    setOrderBy(orderBy.filter((_, i) => i !== index))
  }

  const handleExecuteReport = async () => {
    if (selectedTables.length === 0 || selectedTables[0].columns.length === 0) {
      alert('Please select at least one table and column')
      return
    }

    setExecutingReport(true)
    try {
      const response = await fetch('http://localhost:8000/api/v1/reports/custom/execute', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          tables: selectedTables,
          filters,
          groupBy,
          orderBy
        })
      })
      
      if (response.ok) {
        const data = await response.json()
        setReportData(data.results || getMockReportData())
      } else {
        // Use mock data
        setReportData(getMockReportData())
      }
    } catch (error) {
      console.error('Failed to execute report:', error)
      setReportData(getMockReportData())
    } finally {
      setExecutingReport(false)
    }
  }

  const getMockReportData = () => {
    // Generate mock data based on selected columns
    if (selectedTables.length === 0 || selectedTables[0].columns.length === 0) return []
    
    const mockData = []
    for (let i = 0; i < 10; i++) {
      const row: any = {}
      selectedTables.forEach(table => {
        table.columns.forEach(col => {
          const key = col.displayName || col.columnName
          if (col.columnName.includes('amount') || col.columnName.includes('price')) {
            row[key] = Math.floor(Math.random() * 10000) + 1000
          } else if (col.columnName.includes('date')) {
            row[key] = new Date(2024, 0, Math.floor(Math.random() * 30) + 1).toISOString().split('T')[0]
          } else if (col.columnName.includes('quantity') || col.columnName.includes('count')) {
            row[key] = Math.floor(Math.random() * 100) + 1
          } else {
            row[key] = `${col.columnName}_${i + 1}`
          }
        })
      })
      mockData.push(row)
    }
    return mockData
  }

  const handleSaveReport = async () => {
    if (!reportName) {
      alert('Please enter a report name')
      return
    }

    try {
      const response = await fetch('http://localhost:8000/api/v1/reports/custom', {
        method: currentReport ? 'PUT' : 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          id: currentReport?.id,
          name: reportName,
          description: reportDescription,
          tables: selectedTables,
          filters,
          groupBy,
          orderBy
        })
      })
      
      if (response.ok) {
        alert('Report saved successfully')
        setShowSaveModal(false)
        setShowReportBuilder(false)
        fetchSavedReports()
      } else {
        alert('Failed to save report')
      }
    } catch (error) {
      console.error('Failed to save report:', error)
      alert('Failed to save report')
    }
  }

  const handleExportReport = (format: 'csv' | 'excel' | 'pdf') => {
    if (reportData.length === 0) {
      alert('No data to export. Please execute the report first.')
      return
    }

    // In a real app, this would generate the file server-side
    console.log(`Exporting report as ${format}:`, reportData)
    alert(`Report exported as ${format.toUpperCase()}`)
  }

  const formatCellValue = (value: any, column: SelectedColumn) => {
    if (value === null || value === undefined) return '-'
    
    switch (column.format) {
      case 'currency':
        return new Intl.NumberFormat('en-US', {
          style: 'currency',
          currency: 'USD'
        }).format(value)
      case 'date':
        return new Date(value).toLocaleDateString()
      case 'percentage':
        return `${value}%`
      case 'number':
        return new Intl.NumberFormat('en-US').format(value)
      default:
        return value.toString()
    }
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Custom Reports"
        description="Build and manage custom reports"
        actions={
          <Button
            onClick={handleNewReport}
            icon={<PlusIcon className="h-4 w-4" />}
          >
            New Report
          </Button>
        }
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports', href: '/reports' },
          { label: 'Custom Reports' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {!showReportBuilder ? (
          // Saved Reports List
          <Card>
            <div className="px-6 py-4 border-b border-gray-200">
              <h3 className="text-lg font-medium text-gray-900">Saved Reports</h3>
            </div>
            
            {loading ? (
              <div className="p-8 text-center">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600 mx-auto"></div>
              </div>
            ) : (
              <div className="overflow-x-auto">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Report Name
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Description
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Last Modified
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Created By
                      </th>
                      <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Actions
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {savedReports.map((report) => (
                      <tr key={report.id} className="hover:bg-gray-50">
                        <td className="px-6 py-4 whitespace-nowrap">
                          <div className="text-sm font-medium text-gray-900">{report.name}</div>
                        </td>
                        <td className="px-6 py-4">
                          <div className="text-sm text-gray-500">{report.description}</div>
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {new Date(report.lastModified).toLocaleDateString()}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                          {report.createdBy}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                          <div className="flex items-center justify-end gap-2">
                            <button
                              onClick={() => handleEditReport(report)}
                              className="text-indigo-600 hover:text-indigo-900"
                            >
                              <PlayIcon className="h-4 w-4" />
                            </button>
                            <button
                              onClick={() => handleDuplicateReport(report)}
                              className="text-gray-600 hover:text-gray-900"
                            >
                              <DocumentDuplicateIcon className="h-4 w-4" />
                            </button>
                            <button
                              onClick={() => handleDeleteReport(report.id)}
                              className="text-red-600 hover:text-red-900"
                            >
                              <TrashIcon className="h-4 w-4" />
                            </button>
                          </div>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}
          </Card>
        ) : (
          // Report Builder
          <div className="space-y-6">
            {/* Report Builder Header */}
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <h3 className="text-lg font-medium text-gray-900">
                    {currentReport ? 'Edit Report' : 'New Report'}
                  </h3>
                  <div className="flex items-center gap-3">
                    <Button
                      variant="ghost"
                      onClick={() => setShowReportBuilder(false)}
                    >
                      Cancel
                    </Button>
                    <Button
                      variant="secondary"
                      onClick={() => setShowSaveModal(true)}
                      icon={<SaveIcon className="h-4 w-4" />}
                    >
                      Save
                    </Button>
                    <Button
                      onClick={handleExecuteReport}
                      loading={executingReport}
                      icon={<PlayIcon className="h-4 w-4" />}
                    >
                      Run Report
                    </Button>
                  </div>
                </div>
              </div>
            </Card>

            {/* Table Selection */}
            <Card>
              <div className="p-6">
                <h4 className="text-sm font-medium text-gray-900 mb-4">Select Tables</h4>
                <div className="grid grid-cols-2 gap-4 mb-4">
                  <Select
                    label="Available Tables"
                    onChange={(e) => handleAddTable(e.target.value)}
                    value=""
                  >
                    <option value="">Select a table to add</option>
                    {Object.entries(availableTables).map(([key, table]) => (
                      <option key={key} value={key}>{table.displayName}</option>
                    ))}
                  </Select>
                </div>

                {/* Selected Tables and Columns */}
                {selectedTables.map((table, tableIndex) => (
                  <div key={tableIndex} className="mt-4 border border-gray-200 rounded-lg p-4">
                    <div className="flex items-center justify-between mb-3">
                      <h5 className="font-medium">{availableTables[table.tableName as keyof typeof availableTables].displayName}</h5>
                      <button
                        onClick={() => setSelectedTables(selectedTables.filter((_, i) => i !== tableIndex))}
                        className="text-red-500 hover:text-red-700"
                      >
                        <XMarkIcon className="h-5 w-5" />
                      </button>
                    </div>
                    
                    <div className="space-y-2">
                      <Select
                        label="Add Column"
                        onChange={(e) => handleAddColumn(tableIndex, e.target.value)}
                        value=""
                      >
                        <option value="">Select a column</option>
                        {availableTables[table.tableName as keyof typeof availableTables].columns.map(col => (
                          <option key={col} value={col}>{col.replace(/_/g, ' ')}</option>
                        ))}
                      </Select>
                      
                      {table.columns.map((column, colIndex) => (
                        <div key={colIndex} className="flex items-center gap-3 bg-gray-50 p-2 rounded">
                          <Input
                            label="Display Name"
                            value={column.displayName || ''}
                            onChange={(e) => {
                              const newTables = [...selectedTables]
                              newTables[tableIndex].columns[colIndex].displayName = e.target.value
                              setSelectedTables(newTables)
                            }}
                            className="flex-1"
                          />
                          <Select
                            label="Format"
                            value={column.format || ''}
                            onChange={(e) => {
                              const newTables = [...selectedTables]
                              newTables[tableIndex].columns[colIndex].format = e.target.value as any
                              setSelectedTables(newTables)
                            }}
                          >
                            <option value="">None</option>
                            <option value="currency">Currency</option>
                            <option value="date">Date</option>
                            <option value="percentage">Percentage</option>
                            <option value="number">Number</option>
                          </Select>
                          <Select
                            label="Aggregate"
                            value={column.aggregate || ''}
                            onChange={(e) => {
                              const newTables = [...selectedTables]
                              newTables[tableIndex].columns[colIndex].aggregate = e.target.value as any
                              setSelectedTables(newTables)
                            }}
                          >
                            <option value="">None</option>
                            <option value="SUM">Sum</option>
                            <option value="AVG">Average</option>
                            <option value="COUNT">Count</option>
                            <option value="MIN">Min</option>
                            <option value="MAX">Max</option>
                          </Select>
                          <button
                            onClick={() => handleRemoveColumn(tableIndex, colIndex)}
                            className="text-red-500 hover:text-red-700"
                          >
                            <XMarkIcon className="h-5 w-5" />
                          </button>
                        </div>
                      ))}
                    </div>
                  </div>
                ))}
              </div>
            </Card>

            {/* Filters */}
            <Card>
              <div className="p-6">
                <div className="flex items-center justify-between mb-4">
                  <h4 className="text-sm font-medium text-gray-900">Filters</h4>
                  <Button
                    size="sm"
                    variant="ghost"
                    onClick={handleAddFilter}
                    icon={<PlusIcon className="h-4 w-4" />}
                  >
                    Add Filter
                  </Button>
                </div>
                
                {filters.map((filter, index) => (
                  <div key={index} className="flex items-center gap-3 mb-3">
                    <Select
                      value={filter.column}
                      onChange={(e) => {
                        const newFilters = [...filters]
                        newFilters[index].column = e.target.value
                        setFilters(newFilters)
                      }}
                    >
                      <option value="">Select Column</option>
                      {selectedTables.flatMap(table => 
                        table.columns.map(col => (
                          <option key={`${table.tableName}.${col.columnName}`} value={`${table.tableName}.${col.columnName}`}>
                            {col.displayName || col.columnName}
                          </option>
                        ))
                      )}
                    </Select>
                    
                    <Select
                      value={filter.operator}
                      onChange={(e) => {
                        const newFilters = [...filters]
                        newFilters[index].operator = e.target.value as any
                        setFilters(newFilters)
                      }}
                    >
                      <option value="equals">Equals</option>
                      <option value="not_equals">Not Equals</option>
                      <option value="greater_than">Greater Than</option>
                      <option value="less_than">Less Than</option>
                      <option value="contains">Contains</option>
                      <option value="between">Between</option>
                    </Select>
                    
                    <Input
                      value={filter.value}
                      onChange={(e) => {
                        const newFilters = [...filters]
                        newFilters[index].value = e.target.value
                        setFilters(newFilters)
                      }}
                      placeholder="Value"
                    />
                    
                    {filter.operator === 'between' && (
                      <Input
                        value={filter.value2 || ''}
                        onChange={(e) => {
                          const newFilters = [...filters]
                          newFilters[index].value2 = e.target.value
                          setFilters(newFilters)
                        }}
                        placeholder="Value 2"
                      />
                    )}
                    
                    <button
                      onClick={() => handleRemoveFilter(index)}
                      className="text-red-500 hover:text-red-700"
                    >
                      <XMarkIcon className="h-5 w-5" />
                    </button>
                  </div>
                ))}
              </div>
            </Card>

            {/* Group By */}
            <Card>
              <div className="p-6">
                <h4 className="text-sm font-medium text-gray-900 mb-4">Group By</h4>
                <Select
                  multiple
                  value={groupBy}
                  onChange={(e) => {
                    const values = Array.from(e.target.selectedOptions, option => option.value)
                    setGroupBy(values)
                  }}
                  className="h-32"
                >
                  {selectedTables.flatMap(table => 
                    table.columns.map(col => (
                      <option key={`${table.tableName}.${col.columnName}`} value={`${table.tableName}.${col.columnName}`}>
                        {col.displayName || col.columnName}
                      </option>
                    ))
                  )}
                </Select>
              </div>
            </Card>

            {/* Order By */}
            <Card>
              <div className="p-6">
                <div className="flex items-center justify-between mb-4">
                  <h4 className="text-sm font-medium text-gray-900">Order By</h4>
                  <Button
                    size="sm"
                    variant="ghost"
                    onClick={handleAddOrderBy}
                    icon={<PlusIcon className="h-4 w-4" />}
                  >
                    Add Sort
                  </Button>
                </div>
                
                {orderBy.map((order, index) => (
                  <div key={index} className="flex items-center gap-3 mb-3">
                    <Select
                      value={order.column}
                      onChange={(e) => {
                        const newOrderBy = [...orderBy]
                        newOrderBy[index].column = e.target.value
                        setOrderBy(newOrderBy)
                      }}
                    >
                      <option value="">Select Column</option>
                      {selectedTables.flatMap(table => 
                        table.columns.map(col => (
                          <option key={`${table.tableName}.${col.columnName}`} value={`${table.tableName}.${col.columnName}`}>
                            {col.displayName || col.columnName}
                          </option>
                        ))
                      )}
                    </Select>
                    
                    <Select
                      value={order.direction}
                      onChange={(e) => {
                        const newOrderBy = [...orderBy]
                        newOrderBy[index].direction = e.target.value as any
                        setOrderBy(newOrderBy)
                      }}
                    >
                      <option value="ASC">Ascending</option>
                      <option value="DESC">Descending</option>
                    </Select>
                    
                    <button
                      onClick={() => handleRemoveOrderBy(index)}
                      className="text-red-500 hover:text-red-700"
                    >
                      <XMarkIcon className="h-5 w-5" />
                    </button>
                  </div>
                ))}
              </div>
            </Card>

            {/* Report Results */}
            {reportData.length > 0 && (
              <Card>
                <div className="px-6 py-4 border-b border-gray-200">
                  <div className="flex items-center justify-between">
                    <h3 className="text-lg font-medium text-gray-900">Report Results</h3>
                    <div className="flex items-center gap-2">
                      <Button
                        size="sm"
                        variant="secondary"
                        onClick={() => handleExportReport('csv')}
                        icon={<ArrowDownTrayIcon className="h-4 w-4" />}
                      >
                        CSV
                      </Button>
                      <Button
                        size="sm"
                        variant="secondary"
                        onClick={() => handleExportReport('excel')}
                        icon={<ArrowDownTrayIcon className="h-4 w-4" />}
                      >
                        Excel
                      </Button>
                      <Button
                        size="sm"
                        variant="secondary"
                        onClick={() => handleExportReport('pdf')}
                        icon={<ArrowDownTrayIcon className="h-4 w-4" />}
                      >
                        PDF
                      </Button>
                    </div>
                  </div>
                </div>
                
                <div className="overflow-x-auto">
                  <table className="min-w-full divide-y divide-gray-200">
                    <thead className="bg-gray-50">
                      <tr>
                        {selectedTables[0]?.columns.map((col, index) => (
                          <th key={index} className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                            {col.displayName || col.columnName}
                          </th>
                        ))}
                      </tr>
                    </thead>
                    <tbody className="bg-white divide-y divide-gray-200">
                      {reportData.map((row, rowIndex) => (
                        <tr key={rowIndex}>
                          {selectedTables[0]?.columns.map((col, colIndex) => (
                            <td key={colIndex} className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                              {formatCellValue(row[col.displayName || col.columnName], col)}
                            </td>
                          ))}
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </Card>
            )}
          </div>
        )}
      </main>

      {/* Save Report Modal */}
      <Modal
        isOpen={showSaveModal}
        onClose={() => setShowSaveModal(false)}
        title="Save Report"
      >
        <div className="space-y-4">
          <Input
            label="Report Name"
            value={reportName}
            onChange={(e) => setReportName(e.target.value)}
            required
          />
          <Input
            label="Description"
            value={reportDescription}
            onChange={(e) => setReportDescription(e.target.value)}
            type="textarea"
            rows={3}
          />
          <div className="flex justify-end gap-3">
            <Button
              variant="ghost"
              onClick={() => setShowSaveModal(false)}
            >
              Cancel
            </Button>
            <Button onClick={handleSaveReport}>
              Save Report
            </Button>
          </div>
        </div>
      </Modal>
    </div>
  )
}